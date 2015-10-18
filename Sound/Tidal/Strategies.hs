{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}

module Sound.Tidal.Strategies where

import Data.Ratio
import Control.Applicative
import qualified Data.Map as Map

import Data.Maybe
import Sound.Tidal.Dirt
import Sound.Tidal.Pattern
import Sound.Tidal.Stream
import Sound.Tidal.Time
import Sound.Tidal.Utils
import Sound.Tidal.Params
import qualified Sound.OSC.FD as OSC.FD

stutter n t p = stack $ map (\i -> (t * (fromIntegral i)) ~> p) [0 .. (n-1)]

echo   = stutter 2
triple = stutter 3
quad   = stutter 4
double = echo

jux f p = stack [p # pan (pure 0), f $ p # pan (pure 1)]
juxcut f p = stack [p     # pan (pure 0) # cut (pure (-1)), 
                    f $ p # pan (pure 1) # cut (pure (-2))
                   ]

jux' fs p = stack $ map (\n -> ((fs !! n) p) # pan (pure $ fromIntegral n / fromIntegral l)) [0 .. l-1]
  where l = length fs

-- For multichannel
jux4 f p = stack [p # pan (pure (5/8)), f $ p # pan (pure (1/8))]

juxBy n f p = stack [p # pan (pure $ 0.5 - (n/2)), f $ p # pan (pure $ 0.5 + (n/2))]

-- every 4 (smash 4 [1, 2, 3]) $ sound "[odx sn/2 [~ odx] sn/3, [~ hh]*4]"

smash n xs p = slowcat $ map (\n -> slow n p') xs
  where p' = striate n p

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

spin :: Int -> OscPattern -> OscPattern
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

inside n f p = density n $ f (slow n p)

scale :: (Functor f, Num b) => b -> b -> f b -> f b
scale from to p = ((+ from) . (* (to-from))) <$> p

chop :: Int -> OscPattern -> OscPattern
chop n p = Pattern $ \queryA -> concatMap (f queryA) $ arcCycles queryA
     where f queryA a = concatMap (chopEvent queryA) (arc p a)
           chopEvent (queryS, queryE) (a,a',v) = map (newEvent v) $ filter (\(_, (s,e)) -> not $ or [e < queryS, s >= queryE]) (enumerate $ chopArc a n)
           newEvent :: OscMap -> (Int, Arc) -> Event OscMap
           newEvent v (i, a) = (a,a,Map.insert (param dirt "end") (Just $ OSC.FD.float ((fromIntegral $ i+1)/(fromIntegral n))) $ Map.insert (param dirt "begin") (Just $ OSC.FD.float ((fromIntegral i)/(fromIntegral n))) v)

gap :: Int -> OscPattern -> OscPattern
gap n p = Pattern $ \queryA -> concatMap (f queryA) $ arcCycles queryA
     where f queryA a = concatMap (chopEvent queryA) (arc p a)
           chopEvent (queryS, queryE) (a,a',v) = map (newEvent v) $ filter (\(_, (s,e)) -> not $ or [e < queryS, s >= queryE]) (enumerate $ everyOther $ chopArc a n)
           newEvent :: OscMap -> (Int, Arc) -> Event OscMap
           newEvent v (i, a) = (a,a,Map.insert (param dirt "end") (Just $ OSC.FD.float ((fromIntegral $ i+1)/(fromIntegral n))) $ Map.insert (param dirt "begin") (Just $ OSC.FD.float ((fromIntegral i)/(fromIntegral n))) v)
           everyOther (x:(y:xs)) = x:(everyOther xs)
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

weave :: Rational -> OscPattern -> [OscPattern] -> OscPattern
weave t p ps = weave' t p (map (\x -> (x #)) ps)

weave' :: Rational -> Pattern a -> [Pattern a -> Pattern a] -> Pattern a
weave' t p fs | l == 0 = silence
              | otherwise = slow t $ stack $ map (\(i, f) -> (fromIntegral i % l) <~ (density t $ f (slow t p))) (zip [0 ..] fs)
  where l = fromIntegral $ length fs

interlace :: OscPattern -> OscPattern -> OscPattern
interlace a b = weave 16 (shape $ ((* 0.9) <$> sinewave1)) [a, b]

-- Step sequencing
step :: String -> String -> Pattern String
step s steps = cat $ map f steps
    where f c | c == 'x' = atom s
              | c >= '0' && c <= '9' = atom $ s ++ ":" ++ [c]
              | otherwise = silence

steps :: [(String, String)] -> Pattern String
steps = stack . map (\(a,b) -> step a b)

off :: Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
off t f p = superimpose (f . (t ~>)) p

offadd :: Num a => Time -> a -> Pattern a -> Pattern a
offadd t n p = off t ((+n) <$>) p

up :: Pattern Double -> OscPattern
up = speed . ((1.059466**) <$>)
