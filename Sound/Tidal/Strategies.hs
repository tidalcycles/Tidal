{-# OPTIONS_GHC -XNoMonomorphismRestriction -XOverloadedStrings #-}

module Sound.Tidal.Strategies where

import Data.Ratio
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Char as Char
import Data.Fixed
import Data.Maybe

import Sound.Tidal.Dirt
import Sound.Tidal.Pattern
import Sound.Tidal.Stream
import Sound.Tidal.Time
import Sound.Tidal.Utils
import Sound.Tidal.Params
import Sound.Tidal.Parse
import Data.List (transpose)

stutter :: Integral i => i -> Time -> Pattern a -> Pattern a
stutter n t p = stack $ map (\i -> (t * (fromIntegral i)) ~> p) [0 .. (n-1)]

echo, triple, quad, double :: Time -> Pattern a -> Pattern a
echo   = stutter 2
triple = stutter 3
quad   = stutter 4
double = echo

{- | The `jux` function creates strange stereo effects, by applying a
function to a pattern, but only in the right-hand channel. For
example, the following reverses the pattern on the righthand side:

@
d1 $ slow 32 $ jux (rev) $ striate' 32 (1/16) $ sound "bev"
@

When passing pattern transforms to functions like [jux](#jux) and [every](#every),
it's possible to chain multiple transforms together with `.`, for
example this both reverses and halves the playback speed of the
pattern in the righthand channel:

@
d1 $ slow 32 $ jux ((# speed "0.5") . rev) $ striate' 32 (1/16) $ sound "bev"
@
-}
jux = juxBy 1
juxcut f p = stack [p     # pan (pure 0) # cut (pure (-1)),
                    f $ p # pan (pure 1) # cut (pure (-2))
                   ]

juxcut' fs p = stack $ map (\n -> ((fs !! n) p |+| cut (pure $ 1-n)) # pan (pure $ fromIntegral n / fromIntegral l)) [0 .. l-1]
  where l = length fs
 
{- | In addition to `jux`, `jux'` allows using a list of pattern transform. resulting patterns from each transformation will be spread via pan from left to right.

For example:

@
d1 $ jux' [iter 4, chop 16, id, rev, palindrome] $ sound "bd sn"
@

will put `iter 4` of the pattern to the far left and `palindrome` to the far right. In the center the original pattern will play and mid left mid right the chopped and the reversed version will appear.

One could also write:

@
d1 $ stack [  
    iter 4 $ sound "bd sn" # pan "0",  
    chop 16 $ sound "bd sn" # pan "0.25",  
    sound "bd sn" # pan "0.5",  
    rev $ sound "bd sn" # pan "0.75",  
    palindrome $ sound "bd sn" # pan "1",  
    ]  
@

-}
jux' fs p = stack $ map (\n -> ((fs !! n) p) # pan (pure $ fromIntegral n / fromIntegral l)) [0 .. l-1]
  where l = length fs

-- | Multichannel variant of `jux`, _not sure what it does_
jux4 f p = stack [p # pan (pure (5/8)), f $ p # pan (pure (1/8))]

{- |
With `jux`, the original and effected versions of the pattern are
panned hard left and right (i.e., panned at 0 and 1). This can be a
bit much, especially when listening on headphones. The variant `juxBy`
has an additional parameter, which brings the channel closer to the
centre. For example:

@
d1 $ juxBy 0.5 (density 2) $ sound "bd sn:1"
@

In the above, the two versions of the pattern would be panned at 0.25
and 0.75, rather than 0 and 1.
-}
juxBy n f p = stack [p |+| pan (pure $ 0.5 - (n/2)), f $ p |+| pan (pure $ 0.5 + (n/2))]

{- | Smash is a combination of `spread` and `striate` - it cuts the samples
into the given number of bits, and then cuts between playing the loop
at different speeds according to the values in the list.

So this:

@
d1 $ smash 3 [2,3,4] $ sound "ho ho:2 ho:3 hc"
@

Is a bit like this:

@
d1 $ spread (slow) [2,3,4] $ striate 3 $ sound "ho ho:2 ho:3 hc"
@

This is quite dancehall:

@
d1 $ (spread' slow "1%4 2 1 3" $ spread (striate) [2,3,4,1] $ sound
"sn:2 sid:3 cp sid:4")
  # speed "[1 2 1 1]/2"
@
-}
smash n xs p = slowcat $ map (\n -> slow n p') xs
  where p' = striate n p

{- | an altenative form to `smash` is `smash'` which will use `chop` instead of `striate`.
-}
smash' n xs p = slowcat $ map (\n -> slow n p') xs
  where p' = chop n p

-- samples "jvbass [~ latibro] [jvbass [latibro jvbass]]" ((1%2) <~ slow 6 "[1 6 8 7 3]")

samples :: Applicative f => f String -> f Int -> f String
samples p p' = pick <$> p <*> p'

samples' :: Applicative f => f String -> f Int -> f String
samples' p p' = (flip pick) <$> p' <*> p

{-
scrumple :: Time -> Pattern a -> Pattern a -> Pattern a
scrumple o p p' = p'' -- overlay p (o ~> p'')
  where p'' = Pattern $ \a -> concatMap
                              (\((s,d), vs) -> map (\x -> ((s,d),
                                                           snd x
                                                          )
                                                   )
                                                   (arc p' (s,s))
                              ) (arc p a)
-}

--rev :: Pattern a -> Pattern a
--rev p = Pattern $ \a -> concatMap
--                        (\a' -> mapFsts mirrorArc $
--                                (arc p (mirrorArc a')))
--                        (arcCycles a)

--spreadf :: [Pattern a -> Pattern b] -> Pattern a -> Pattern b
spreadf ts p = spread ($)

{- | `spin` will "spin" a layer up a pattern the given number of times, with each successive layer offset in time by an additional `1/n` of a cycle, and panned by an additional `1/n`. The result is a pattern that seems to spin around. This function works best on multichannel systems.

@
d1 $ slow 3 $ spin 4 $ sound "drum*3 tabla:4 [arpy:2 ~ arpy] [can:2 can:3]"
@
-}
spin :: Int -> ParamPattern -> ParamPattern
spin copies p =
  stack $ map (\n -> let offset = toInteger n % toInteger copies in
                     offset <~ p
                     # pan (pure $ fromRational offset)
              )
          [0 .. (copies - 1)]

{-stripe :: Arc -> Pattern a -> Pattern a
stripe (stripeS, stripeE) p = slow t $ Pattern $ \a -> concatMap f $ arcCycles a
  where f a = mapFsts (stretch . stripe') $ arc p (stripe' a)
        trunc' (s,e) = (min s ((sam s) + t), min e ((sam s) + t))
        stretch (s,e) = (sam s + ((s - sam s) / t), sam s + ((e - sam s) / t))
-}

sawwave4, sinewave4, rand4 :: Pattern Double
sawwave4 = ((*4) <$> sawwave1)
sinewave4 = ((*4) <$> sinewave1)
rand4 = ((*4) <$> rand)

stackwith p ps | null ps = silence
               | otherwise = stack $ map (\(i, p') -> p' # (((fromIntegral i) % l) <~ p)) (zip [0 ..] ps)
  where l = fromIntegral $ length ps

{-
cross f p p' = Pattern $ \t -> concat [filter flt $ arc p t,
                                       filter (not . flt) $ arc p' t
                                      ]
]  where flt = f . cyclePos . fst . fst
-}

{- | `scale` will take a pattern which goes from 0 to 1 (like `sine1`), and scale it to a different range - between the first and second arguments. In the below example, `scale 1 1.5` shifts the range of `sine1` from 0 - 1 to 1 - 1.5.

@
d1 $ jux (iter 4) $ sound "arpy arpy:2*2"
  |+| speed (slow 4 $ scale 1 1.5 sine1)
@
-}
scale :: (Functor f, Num b) => b -> b -> f b -> f b
scale from to p = ((+ from) . (* (to-from))) <$> p

{- | `scalex` is an exponential version of `scale`, good for using with
frequencies.  Do *not* use negative numbers or zero as arguments! -}
scalex :: (Functor f, Floating b) => b -> b -> f b -> f b
scalex from to p = exp <$> scale (log from) (log to) p

{- | `chop` granualizes every sample in place as it is played, turning a pattern of samples into a pattern of sample parts. Use an integer value to specify how many granules each sample is chopped into:

@
d1 $ chop 16 $ sound "arpy arp feel*4 arpy*4"
@

Different values of `chop` can yield very different results, depending
on the samples used:


@
d1 $ chop 16 $ sound (samples "arpy*8" (run 16))
d1 $ chop 32 $ sound (samples "arpy*8" (run 16))
d1 $ chop 256 $ sound "bd*4 [sn cp] [hh future]*2 [cp feel]"
@
-}
chop :: Int -> ParamPattern -> ParamPattern
chop n p = Pattern $ \queryA -> concatMap (f queryA) $ arcCycles queryA
     where f queryA a = concatMap (chopEvent queryA) (arc p a)
           chopEvent (queryS, queryE) (a,_a',v) = map (newEvent v) $ filter (\(_, (s,e)) -> not $ or [e < queryS, s >= queryE]) (enumerate $ chopArc a n)
           newEvent :: ParamMap -> (Int, Arc) -> Event ParamMap
           newEvent v (i, a) = (a,a,Map.insert (param dirt "end") (VF ((fromIntegral $ i+1)/(fromIntegral n))) $ Map.insert (param dirt "begin") (VF ((fromIntegral i)/(fromIntegral n))) v)

{- | `gap` is similar to `chop` in that it granualizes every sample in place as it is played,
but every other grain is silent. Use an integer value to specify how many granules
each sample is chopped into:

@
d1 $ gap 8 $ sound "jvbass"
d1 $ gap 16 $ sound "[jvbass drum:4]"
@-}
gap :: Int -> ParamPattern -> ParamPattern
gap n p = Pattern $ \queryA -> concatMap (f queryA) $ arcCycles queryA
     where f queryA a = concatMap (chopEvent queryA) (arc p a)
           chopEvent (queryS, queryE) (a,_a',v) = map (newEvent v) $ filter (\(_, (s,e)) -> not $ or [e < queryS, s >= queryE]) (enumerate $ everyOther $ chopArc a n)
           newEvent :: ParamMap -> (Int, Arc) -> Event ParamMap
           newEvent v (i, a) = (a,a,Map.insert (param dirt "end") (VF ((fromIntegral $ i+1)/(fromIntegral n))) $ Map.insert (param dirt "begin") (VF ((fromIntegral i)/(fromIntegral n))) v)
           everyOther (x:_:xs) = x:everyOther xs
           everyOther xs = xs

chopArc :: Arc -> Int -> [Arc]
chopArc (s, e) n = map (\i -> ((s + (e-s)*(fromIntegral i/fromIntegral n)), s + (e-s)*((fromIntegral $ i+1)/fromIntegral n))) [0 .. n-1]
{-
normEv :: Event a -> Event a -> Event a
normEv ev@(_, (s,e), _) ev'@(_, (s',e'), _)
       | not on && not off = [] -- shouldn't happen
       | on && off = splitEv ev'
       | not on && s' > sam s = []
       | not off && e' < nextSam s = [(fst' ev, mapSnd' (mapSnd (min $ nextSam s)) ev, thd' ev)]
  where on = onsetIn (sam s, nextSam s) ev
        off = offsetIn (sam s, nextSam s) ev
        eplitEv
-}
--mapCycleEvents :: Pattern a -> ([Event a] -> [Event a]) -> Pattern a
--mapCycleEvents p f = splitQueries $ Pattern $ \(s,e) -> filter (\ev -> isJust $ subArc (s,e) (eventArc ev)) $ f $ arc p (sam s, nextSam s)

--off :: Time -> Pattern a -> Pattern a
--off t p = mapCycleEvents p (mapArcs (mapSnd wrappedPlus . mapFst wrappedPlus))
--               where wrapAtCycle f t' = sam t' + cyclePos (f t')
--                     wrappedPlus = wrapAtCycle (+t)


en :: [(Int, Int)] -> Pattern String -> Pattern String
en ns p = stack $ map (\(i, (k, n)) -> e k n (samples p (pure i))) $ enumerate ns

{- |
`weave` applies a function smoothly over an array of different patterns. It uses an `OscPattern` to
apply the function at different levels to each pattern, creating a weaving effect.

@
d1 $ weave 3 (shape $ sine1) [sound "bd [sn drum:2*2] bd*2 [sn drum:1]", sound "arpy*8 ~"]
@
-}
weave :: Rational -> ParamPattern -> [ParamPattern] -> ParamPattern
weave t p ps = weave' t p (map (\x -> (x #)) ps)


{- | `weave'` is similar in that it blends functions at the same time at different amounts over a pattern:

@
d1 $ weave' 3 (sound "bd [sn drum:2*2] bd*2 [sn drum:1]") [density 2, (# speed "0.5"), chop 16]
@
-}
weave' :: Rational -> Pattern a -> [Pattern a -> Pattern a] -> Pattern a
weave' t p fs | l == 0 = silence
              | otherwise = slow t $ stack $ map (\(i, f) -> (fromIntegral i % l) <~ (density t $ f (slow t p))) (zip [0 ..] fs)
  where l = fromIntegral $ length fs

{- | 
(A function that takes two OscPatterns, and blends them together into
a new OscPattern. An OscPattern is basically a pattern of messages to
a synthesiser.)

Shifts between the two given patterns, using distortion.

Example:

@
d1 $ interlace (sound  "bd sn kurt") (every 3 rev $ sound  "bd sn:2")
@
-}
interlace :: ParamPattern -> ParamPattern -> ParamPattern
interlace a b = weave 16 (shape $ ((* 0.9) <$> sinewave1)) [a, b]

-- | Step sequencing
step :: String -> String -> Pattern String
step s steps = cat $ map f steps
    where f c | c == 'x' = atom s
              | c >= '0' && c <= '9' = atom $ s ++ ":" ++ [c]
              | otherwise = silence

steps :: [(String, String)] -> Pattern String
steps = stack . map (\(a,b) -> step a b)

-- | like `step`, but allows you to specify an array of strings to use for 0,1,2...
step' :: [String] -> String -> Pattern String
step' ss steps = cat $ map f steps
    where f c | c == 'x' = atom $ ss!!0
              | c >= '0' && c <= '9' = atom $ ss!!(Char.digitToInt c)
              | otherwise = silence

off :: Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
off t f p = superimpose (f . (t ~>)) p

offadd :: Num a => Time -> a -> Pattern a -> Pattern a
offadd t n p = off t ((+n) <$>) p

{- | `up` does a poor man's pitchshift by semitones via `speed`.

You can easily produce melodies from a single sample with up:

@
d1 # up "0 5 4 12" # sound "arpy"
@

This will play the _arpy_ sample four times a cycle in the original pitch, pitched by 5 semitones, by 4 and then by an octave.
-}
up :: Pattern Double -> ParamPattern
up = speed . ((1.059466**) <$>)

ghost'' a f p = superimpose (((a*2.5) ~>) . f) $ superimpose (((a*1.5) ~>) . f) $ p
ghost' a p = ghost'' 0.125 ((|*| gain (pure 0.7)) . (|=| end (pure 0.2)) . (|*| speed (pure 1.25))) p
ghost p = ghost' 0.125 p 

slice :: Int -> Int -> ParamPattern -> ParamPattern
slice i n p = 
      p
      # begin (pure $ fromIntegral i / fromIntegral n)
      # end (pure $ fromIntegral (i+1) / fromIntegral n)

randslice :: Int -> ParamPattern -> ParamPattern
randslice n p = unwrap $ (\i -> slice i n p) <$> irand n

{- |
`loopAt` makes a sample fit the given number of cycles. Internally, it
works by setting the `unit` parameter to "c", changing the playback
speed of the sample with the `speed` parameter, and setting setting
the `density` of the pattern to match.

@
d1 $ loopAt 4 $ sound "breaks125"
d1 $ juxBy 0.6 (|*| speed "2") $ slowspread (loopAt) [4,6,2,3] $ chop 12 $ sound "fm:14"
@ 
-}
loopAt :: Time -> ParamPattern -> ParamPattern
loopAt n p = slow n p |*| speed (pure $ fromRational $ 1/n) # unit (pure "c")


{- |
   tabby - A more literal weaving than the `weave` function, give number
   of 'threads' per cycle and two patterns, and this function will weave them
   together using a plain (aka 'tabby') weave, with a simple over/under structure
 -}
tabby n p p' = stack [maskedWarp n p,
                      maskedWeft n p'
                     ]
  where             
    weft n = concatMap (\x -> [[0..n-1],(reverse [0..n-1])]) [0 .. (n `div` 2) - 1]
    warp = transpose . weft
    thread xs n p = slow (n%1) $ cat $ map (\i -> zoom (i%n,(i+1)%n) p) (concat xs)
    weftP n p = thread (weft n) n p
    warpP n p = thread (warp n) n p
    maskedWeft n p = Sound.Tidal.Pattern.mask (every 2 rev $ density ((n)%2) "~ 1" :: Pattern Int) $ weftP n p
    maskedWarp n p = mask (every 2 rev $ density ((n)%2) "1 ~" :: Pattern Int) $ warpP n p
