{-# LANGUAGE DeriveDataTypeable #-}

module Sound.Tidal.Pattern where

import Control.Applicative
import Data.Monoid
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Debug.Trace
import Data.Typeable
import Data.Function
import System.Random.Mersenne.Pure64

import Music.Theory.Bjorklund

import Sound.Tidal.Time
import Sound.Tidal.Utils

-- | The pattern datatype, a function from a time @Arc@ to @Event@
-- values. For discrete patterns, this returns the events which are
-- active during that time. For continuous patterns, events with
-- values for the midpoint of the given @Arc@ is returned.
data Pattern a = Pattern {arc :: Arc -> [Event a]}

-- | @show (p :: Pattern)@ returns a text string representing the
-- event values active during the first cycle of the given pattern.
instance (Show a) => Show (Pattern a) where
  show p@(Pattern _) = intercalate " " $ map showEvent $ arc p (0, 1)

showTime t | denominator t == 1 = show (numerator t)
           | otherwise = show (numerator t) ++ ('/':show (denominator t))

showArc a = concat[showTime $ fst a, (' ':showTime (snd a))]

showEvent (a, b, v) | a == b = concat["(",show v,
                                      (' ':showArc a),
                                      ")"
                                     ]
                    | otherwise = show v

instance Functor Pattern where
  fmap f (Pattern a) = Pattern $ fmap (fmap (mapThd' f)) a

-- | @pure a@ returns a pattern with an event with value @a@, which
-- has a duration of one cycle, and repeats every cycle.
instance Applicative Pattern where
  pure x = Pattern $ \(s, e) -> map
                                (\t -> ((t%1, (t+1)%1),
                                        (t%1, (t+1)%1),
                                        x
                                       )
                                )
                                [floor s .. ((ceiling e) - 1)]
  (Pattern fs) <*> (Pattern xs) =
    Pattern $ \a -> concatMap applyX (fs a)
    where applyX ((s,e), (s', e'), f) =
            map (\(_, _, x) -> ((s,e), (s', e'), f x))
                (filter
                 (\(_, a', _) -> isIn a' s)
                 (xs (s',e'))
                )

-- | @mempty@ is a synonym for @silence@.
-- | @mappend@ is a synonym for @overlay@.
instance Monoid (Pattern a) where
    mempty = silence
    mappend = overlay

instance Monad Pattern where
  return = pure
  -- Pattern a -> (a -> Pattern b) -> Pattern b
  -- Pattern Char -> (Char -> Pattern String) -> Pattern String
  
  p >>= f = -- unwrap (f <$> p)
    Pattern (\a -> concatMap
                   (\((s,e), (s',e'), x) -> map (\ev -> ((s,e), (s',e'), thd' ev)) $
                                            filter
                                            (\(a', _, _) -> isIn a' s)
                                            (arc (f x) (s,e))
                   )
                   (arc p a)
             )

-- join x = x >>= id


-- Take a pattern, and function from elements in the pattern to another pattern,
-- and then return that pattern
--bind :: Pattern a -> (a -> Pattern b) -> Pattern b
--bind p f =

-- this is actually join
unwrap :: Pattern (Pattern a) -> Pattern a
unwrap p = Pattern $ \a -> concatMap ((\p' -> arc p' a) . thd') (arc p a)

-- | @atom@ is a synonym for @pure@.
atom :: a -> Pattern a
atom = pure

-- | @silence@ returns a pattern with no events.
silence :: Pattern a
silence = Pattern $ const []

-- | @withQueryArc f p@ returns a new @Pattern@ with function @f@
-- applied to the @Arc@ values passed to the original @Pattern@ @p@.
withQueryArc :: (Arc -> Arc) -> Pattern a -> Pattern a
withQueryArc f p = Pattern $ \a -> arc p (f a)

-- | @withQueryTime f p@ returns a new @Pattern@ with function @f@
-- applied to the both the start and end @Time@ of the @Arc@ passed to
-- @Pattern@ @p@.
withQueryTime :: (Time -> Time) -> Pattern a -> Pattern a
withQueryTime = withQueryArc . mapArc

-- | @withResultArc f p@ returns a new @Pattern@ with function @f@
-- applied to the @Arc@ values in the events returned from the
-- original @Pattern@ @p@.
withResultArc :: (Arc -> Arc) -> Pattern a -> Pattern a
withResultArc f p = Pattern $ \a -> mapArcs f $ arc p a

-- | @withResultTime f p@ returns a new @Pattern@ with function @f@
-- applied to the both the start and end @Time@ of the @Arc@ values in
-- the events returned from the original @Pattern@ @p@.
withResultTime :: (Time -> Time) -> Pattern a -> Pattern a
withResultTime = withResultArc . mapArc

-- | @overlay@ combines two @Pattern@s into a new pattern, so that
-- their events are combined over time. This is the same as the infix
-- operator `<>`.
overlay :: Pattern a -> Pattern a -> Pattern a
overlay p p' = Pattern $ \a -> (arc p a) ++ (arc p' a)

-- | @stack@ combines a list of @Pattern@s into a new pattern, so that
-- their events are combined over time.
stack :: [Pattern a] -> Pattern a
stack ps = foldr overlay silence ps

-- | @append@ combines two patterns @Pattern@s into a new pattern, so
-- that the events of the second pattern are appended to those of the
-- first pattern, within a single cycle

append :: Pattern a -> Pattern a -> Pattern a
append a b = cat [a,b]

-- | @append'@ does the same as @append@, but over two cycles, so that
-- the cycles alternate between the two patterns.
append' :: Pattern a -> Pattern a -> Pattern a
append' a b  = slow 2 $ cat [a,b]

-- | @cat@ returns a new pattern which interlaces the cycles of the
-- given patterns, within a single cycle. It's the equivalent of
-- @append@, but with a list of patterns.
cat :: [Pattern a] -> Pattern a
cat ps = density (fromIntegral $ length ps) $ slowcat ps


splitAtSam :: Pattern a -> Pattern a
splitAtSam p =
  splitQueries $ Pattern $ \(s,e) -> mapSnds' (trimArc (sam s)) $ arc p (s,e)
  where trimArc s' (s,e) = (max (s') s, min (s'+1) e)

-- | @slowcat@ does the same as @cat@, but maintaining the duration of
-- the original patterns. It is the equivalent of @append'@, but with
-- a list of patterns.

slowcat :: [Pattern a] -> Pattern a
slowcat [] = silence
slowcat ps = splitQueries $ Pattern f
  where ps' = map splitAtSam ps
        l = length ps'
        f (s,e) = arc (withResultTime (+offset) p) (s',e')
          where p = ps' !! n
                r = (floor s) :: Int
                n = (r `mod` l) :: Int
                offset = (fromIntegral $ r - ((r - n) `div` l)) :: Time
                (s', e') = (s-offset, e-offset)

-- | @listToPat@ turns the given list of values to a Pattern, which
-- cycles through the list.
listToPat :: [a] -> Pattern a
listToPat = cat . map atom

-- | @maybeListToPat@ is similar to @listToPat@, but allows values to
-- be optional using the @Maybe@ type, so that @Nothing@ results in
-- gaps in the pattern.
maybeListToPat :: [Maybe a] -> Pattern a
maybeListToPat = cat . map f
  where f Nothing = silence
        f (Just x) = atom x

-- | @run@ @n@ returns a pattern representing a cycle of numbers from @0@ to @n-1@.
run n = listToPat [0 .. n-1]
scan n = cat $ map run [1 .. n]

-- | @density@ returns the given pattern with density increased by the
-- given @Time@ factor. Therefore @density 2 p@ will return a pattern
-- that is twice as fast, and @density (1%3) p@ will return one three
-- times as slow.
density :: Time -> Pattern a -> Pattern a
density 0 p = silence
density 1 p = p
density r p = withResultTime (/ r) $ withQueryTime (* r) p


-- | @densityGap@ is similar to @density@ but maintains its cyclic
-- alignment. For example, @densityGap 2 p@ would squash the events in
-- pattern @p@ into the first half of each cycle (and the second
-- halves would be empty).
densityGap :: Time -> Pattern a -> Pattern a
densityGap 0 p = silence
densityGap r p = splitQueries $ withResultArc (\(s,e) -> (sam s + ((s - sam s)/r), (sam s + ((e - sam s)/r)))) $ Pattern (\a -> arc p $ mapArc (\t -> sam t + (min 1 (r * cyclePos t))) a)

-- | @slow@ does the opposite of @density@, i.e. @slow 2 p@ will
-- return a pattern that is half the speed.
slow :: Time -> Pattern a -> Pattern a
slow 0 = id
slow t = density (1/t)

-- | The @<~@ operator shifts (or rotates) a pattern to the left (or
-- counter-clockwise) by the given @Time@ value. For example
-- @(1%16) <~ p@ will return a pattern with all the events moved
-- one 16th of a cycle to the left.
(<~) :: Time -> Pattern a -> Pattern a
(<~) t p = withResultTime (subtract t) $ withQueryTime (+ t) p

-- | The @~>@ operator does the same as @~>@ but shifts events to the
-- right (or clockwise) rather than to the left.
(~>) :: Time -> Pattern a -> Pattern a
(~>) = (<~) . (0-)

brak :: Pattern a -> Pattern a
brak = when ((== 1) . (`mod` 2)) (((1%4) ~>) . (\x -> cat [x, silence]))

iter :: Int -> Pattern a -> Pattern a
iter n p = slowcat $ map (\i -> ((fromIntegral i)%(fromIntegral n)) <~ p) [0 .. n]

-- | @rev p@ returns @p@ with the event positions in each cycle
-- reversed (or mirrored).
rev :: Pattern a -> Pattern a
rev p = splitQueries $ Pattern $ \a -> mapArcs mirrorArc (arc p (mirrorArc a))

-- | @palindrome p@ applies @rev@ to @p@ every other cycle, so that
-- the pattern alternates between forwards and backwards.
palindrome p = append' p (rev p)

-- | @when test f p@ applies the function @f@ to @p@, but in a way
-- which only affects cycles where the @test@ function applied to the
-- cycle number returns @True@.
when :: (Int -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
when test f p = splitQueries $ Pattern apply
  where apply a | test (floor $ fst a) = (arc $ f p) a
                | otherwise = (arc p) a

whenT :: (Time -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
whenT test f p = splitQueries $ Pattern apply
  where apply a | test (fst a) = (arc $ f p) a
                | otherwise = (arc p) a

playWhen :: (Time -> Bool) -> Pattern a -> Pattern a
playWhen test (Pattern f) = Pattern $ (filter (\e -> test (eventOnset e))) . f

playFor :: Time -> Time -> Pattern a -> Pattern a
playFor s e = playWhen (\t -> and [t >= s, t < e])

seqP :: [(Time, Time, Pattern a)] -> Pattern a
seqP = stack . (map (\(s, e, p) -> playFor s e ((sam s) ~> p)))

-- | @every n f p@ applies the function @f@ to @p@, but only affects
-- every @n@ cycles.
every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every 0 f p = p
every n f p = when ((== 0) . (`mod` n)) f p

-- | @foldEvery ns f p@ applies the function @f@ to @p@, and is applied for
-- each cycle in @ns@.
foldEvery :: [Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
foldEvery ns f p = foldr ($) p (map (\x -> every x f) ns)

-- | @sig f@ takes a function from time to values, and turns it into a
-- @Pattern@.
sig :: (Time -> a) -> Pattern a
sig f = Pattern f'
  where f' (s,e) | s > e = []
                 | otherwise = [((s,e), (s,e), f s)]

-- | @sinewave@ returns a @Pattern@ of continuous @Double@ values following a
-- sinewave with frequency of one cycle, and amplitude from -1 to 1.
sinewave :: Pattern Double
sinewave = sig $ \t -> sin $ pi * 2 * (fromRational t)
-- | @sine@ is a synonym for @sinewave.
sine = sinewave
-- | @sinerat@ is equivalent to @sinewave@ for @Rational@ values,
-- suitable for use as @Time@ offsets.
sinerat = fmap toRational sine
ratsine = sinerat

-- | @sinewave1@ is equivalent to @sinewave@, but with amplitude from 0 to 1.
sinewave1 :: Pattern Double
sinewave1 = fmap ((/ 2) . (+ 1)) sinewave

-- | @sine1@ is a synonym for @sinewave1@.
sine1 = sinewave1

-- | @sinerat1@ is equivalent to @sinerat@, but with amplitude from 0 to 1.
sinerat1 = fmap toRational sine1

-- | @sineAmp1 d@ returns @sinewave1@ with its amplitude offset by @d@.
sineAmp1 :: Double -> Pattern Double
sineAmp1 offset = (+ offset) <$> sinewave1

-- | @sawwave@ is the equivalent of @sinewave@ for sawtooth waves.
sawwave :: Pattern Double
sawwave = ((subtract 1) . (* 2)) <$> sawwave1

-- | @saw@ is a synonym for @sawwave@.
saw = sawwave

-- | @sawrat@ is the same as @sawwave@ but returns @Rational@ values
-- suitable for use as @Time@ offsets.
sawrat = fmap toRational saw

sawwave1 :: Pattern Double
sawwave1 = sig $ \t -> mod' (fromRational t) 1
saw1 = sawwave1
sawrat1 = fmap toRational saw1

-- | @triwave@ is the equivalent of @sinewave@ for triangular waves.
triwave :: Pattern Double
triwave = ((subtract 1) . (* 2)) <$> triwave1

-- | @tri@ is a synonym for @triwave@.
tri = triwave

-- | @trirat@ is the same as @triwave@ but returns @Rational@ values
-- suitable for use as @Time@ offsets.
trirat = fmap toRational tri

triwave1 :: Pattern Double
triwave1 = append sawwave1 (rev sawwave1)

tri1 = triwave1
trirat1 = fmap toRational tri1

-- todo - triangular waves again

squarewave1 :: Pattern Double
squarewave1 = sig $
              \t -> fromIntegral $ floor $ (mod' (fromRational t) 1) * 2
square1 = squarewave1

squarewave :: Pattern Double
squarewave = ((subtract 1) . (* 2)) <$> squarewave1
square = squarewave

-- | @envL@ is a @Pattern@ of continuous @Double@ values, representing
-- a linear interpolation between 0 and 1 during the first cycle, then
-- staying constant at 1 for all following cycles. Possibly only
-- useful if you're using something like the retrig function defined
-- in tidal.el.
envL :: Pattern Double
envL = sig $ \t -> max 0 $ min (fromRational t) 1

-- like envL but reversed.
envLR :: Pattern Double
envLR = (1-) <$> envL

-- 'Equal power' for gain-based transitions
envEq :: Pattern Double
envEq = sig $ \t -> sqrt (sin (pi/2 * (max 0 $ min (fromRational (1-t)) 1)))
-- Equal power reversed
envEqR = sig $ \t -> sqrt (cos (pi/2 * (max 0 $ min (fromRational (1-t)) 1)))

fadeOut :: Time -> Pattern a -> Pattern a
fadeOut n = spread' (degradeBy) (slow n $ envL)

-- Alternate versions where you can provide the time from which the fade starts
fadeOut' :: Time -> Time -> Pattern a -> Pattern a
fadeOut' from dur p = spread' (degradeBy) (from ~> slow dur envL) p

-- The 1 <~ is so fade ins and outs have different degredations
fadeIn' :: Time -> Time -> Pattern a -> Pattern a
fadeIn' from dur p = spread' (\n p -> 1 <~ degradeBy n p) (from ~> slow dur ((1-) <$> envL)) p

fadeIn :: Time -> Pattern a -> Pattern a
fadeIn n = spread' (degradeBy) (slow n $ (1-) <$> envL)

spread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
spread f xs p = cat $ map (\x -> f x p) xs

slowspread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
slowspread f xs p = slowcat $ map (\x -> f x p) xs

spread' :: (a -> Pattern b -> Pattern c) -> Pattern a -> Pattern b -> Pattern c
spread' f timepat pat =
  Pattern $ \r -> concatMap (\(_,r', x) -> (arc (f x pat) r')) (rs r)
  where rs r = arc (filterOnsetsInRange timepat) r

filterValues :: (a -> Bool) -> Pattern a -> Pattern a
filterValues f (Pattern x) = Pattern $ (filter (f . thd')) . x

-- Filter out events that have had their onsets cut off
filterOnsets :: Pattern a -> Pattern a
filterOnsets (Pattern f) =
  Pattern $ (filter (\e -> eventOnset e >= eventStart e)) . f

-- Filter events which have onsets, which are within the given range
filterStartInRange :: Pattern a -> Pattern a
filterStartInRange (Pattern f) =
  Pattern $ \(s,e) -> filter ((>= s) . eventOnset) $ f (s,e)

filterOnsetsInRange = filterOnsets . filterStartInRange

seqToRelOnsets :: Arc -> Pattern a -> [(Double, a)]
seqToRelOnsets (s, e) p = map (\((s', _), _, x) -> (fromRational $ (s'-s) / (e-s), x)) $ arc (filterOnsetsInRange p) (s, e)

segment :: Pattern a -> Pattern [a]
segment p = Pattern $ \(s,e) -> filter (\(_,(s',e'),_) -> s' < e && e' > s) $ groupByTime (segment' (arc p (s,e)))

segment' :: [Event a] -> [Event a]
segment' es = foldr split es pts
  where pts = nub $ points es

split :: Time -> [Event a] -> [Event a]
split _ [] = []
split t ((ev@(a,(s,e), v)):es) | t > s && t < e = (a,(s,t),v):(a,(t,e),v):(split t es)
                               | otherwise = ev:split t es

points :: [Event a] -> [Time]
points [] = []
points ((_,(s,e), _):es) = s:e:(points es)

groupByTime :: [Event a] -> [Event [a]]
groupByTime es = map mrg $ groupBy ((==) `on` snd') $ sortBy (compare `on` snd') es
  where mrg es@((a, a', _):_) = (a, a', map thd' es)

ifp :: (Int -> Bool) -> (Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
ifp test f1 f2 p = splitQueries $ Pattern apply
  where apply a | test (floor $ fst a) = (arc $ f1 p) a
                | otherwise = (arc $ f2 p) a

rand :: Pattern Double
rand = Pattern $ \a -> [(a, a, timeToRand $ (midPoint a))]

timeToRand t = fst $ randomDouble $ pureMT $ floor $ (*1000000) t

irand :: Double -> Pattern Int
irand i = (floor . (*i)) <$> rand

degradeBy :: Double -> Pattern a -> Pattern a
degradeBy x p = unMaybe $ (\a f -> toMaybe (f > x) a) <$> p <*> rand
    where toMaybe False _ = Nothing
          toMaybe True a  = Just a
          unMaybe = (fromJust <$>) . filterValues isJust

unDegradeBy :: Double -> Pattern a -> Pattern a
unDegradeBy x p = unMaybe $ (\a f -> toMaybe (f <= x) a) <$> p <*> rand
    where toMaybe False _ = Nothing
          toMaybe True a  = Just a
          unMaybe = (fromJust <$>) . filterValues isJust

sometimesBy :: Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
sometimesBy x f p = overlay (degradeBy x p) (f $ unDegradeBy x p)

sometimes = sometimesBy 0.5
often = sometimesBy 0.75
rarely = sometimesBy 0.25
almostNever = sometimesBy 0.1
almostAlways = sometimesBy 0.9

degrade :: Pattern a -> Pattern a
degrade = degradeBy 0.5

-- | @wedge t p p'@ combines patterns @p@ and @p'@ by squashing the
-- @p@ into the portion of each cycle given by @t@, and @p'@ into the
-- remainer of each cycle.
wedge :: Time -> Pattern a -> Pattern a -> Pattern a
wedge t p p' = overlay (densityGap (1/t) p) (t ~> densityGap (1/(1-t)) p')

whenmod :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
whenmod a b = Sound.Tidal.Pattern.when ((\t -> (t `mod` a) >= b ))

superimpose f p = stack [p, f p]

-- | @splitQueries p@ wraps `p` to ensure that it does not get
-- queries that span arcs. For example `arc p (0.5, 1.5)` would be
-- turned into two queries, `(0.5,1)` and `(1,1.5)`, and the results
-- combined. Being able to assume queries don't span cycles often
-- makes transformations easier to specify.
splitQueries :: Pattern a -> Pattern a
splitQueries p = Pattern $ \a -> concatMap (arc p) $ arcCycles a

trunc :: Time -> Pattern a -> Pattern a
trunc t p = slow t $ splitQueries $ p'
  where p' = Pattern $ \a -> mapArcs (stretch . trunc') $ arc p (trunc' a)
        trunc' (s,e) = (min s ((sam s) + t), min e ((sam s) + t))
        stretch (s,e) = (sam s + ((s - sam s) / t), sam s + ((e - sam s) / t))

zoom :: Arc -> Pattern a -> Pattern a
zoom a@(s,e) p = splitQueries $ withResultArc (mapCycle ((/d) . (subtract s))) $ withQueryArc (mapCycle ((+s) . (*d))) p
     where d = e-s

compress :: Arc -> Pattern a -> Pattern a
compress a@(s,e) p | s >= e = silence
                   | otherwise = s ~> densityGap (1/(e-s)) p

sliceArc :: Arc -> Pattern a -> Pattern a
sliceArc a@(s,e) p | s >= e = silence
                   | otherwise = compress a $ zoom a p

-- @within@ uses @compress@ and @zoom to apply @f@ to only part of pattern @p@
-- for example, @within (1%2) (3%4) ((1%8) <~) "bd sn bd cp"@ would shift only
-- the second @bd@
within :: Arc -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
within (s,e) f p = stack [sliceArc (0,s) p,
                           compress (s,e) $ f $ zoom (s,e) p,
                           sliceArc (e,1) p
                          ]

revArc a = within a rev

e :: Int -> Int -> Pattern a -> Pattern a
e n k p = (flip const) <$> (filterValues (== True) $ listToPat $ bjorklund (n,k)) <*> p

e' :: Int -> Int -> Pattern a -> Pattern a
e' n k p = cat $ map (\x -> if x then p else silence) (bjorklund (n,k))


index :: Real b => b -> Pattern b -> Pattern c -> Pattern c
index sz indexpat pat = spread' (zoom' $ toRational sz) (toRational . (*(1-sz)) <$> indexpat) pat
  where zoom' sz start = zoom (start, start+sz)

-- | @prr rot (blen, vlen) beatPattern valuePattern@: pattern rotate/replace.
prrw :: (a -> b -> c) -> Int -> (Time, Time) -> Pattern a -> Pattern b -> Pattern c
prrw f rot (blen, vlen) beatPattern valuePattern =
  let
    ecompare (_,e1,_) (_,e2,_) = compare (fst e1) (fst e2)
    beats  = sortBy ecompare $ arc beatPattern (0, blen)
    values = fmap thd' . sortBy ecompare $ arc valuePattern (0, vlen)
    cycles = blen * (fromIntegral $ lcm (length beats) (length values) `div` (length beats))
  in
    slow cycles $ stack $ zipWith
    (\( _, (start, end), v') v -> (start ~>) $ densityGap (1 / (end - start)) $ pure (f v' v))
    (sortBy ecompare $ arc (density cycles $ beatPattern) (0, blen))
    (drop (rot `mod` length values) $ cycle values)

-- | @prr rot (blen, vlen) beatPattern valuePattern@: pattern rotate/replace.
prr :: Int -> (Time, Time) -> Pattern a -> Pattern a -> Pattern a
prr = prrw $ flip const

{-|
@preplace (blen, plen) beats values@ combines the timing of @beats@ with the values
of @values@. Other ways of saying this are:
* sequential convolution
* @values@ quantized to @beats@.

Examples:
@
d1 $ sound $ preplace (1,1) "x [~ x] x x" "bd sn"
d1 $ sound $ preplace (1,1) "x(3,8)" "bd sn"
d1 $ sound $ "x(3,8)" <~> "bd sn"
d1 $ sound "[jvbass jvbass:5]*3" |+| (shape $ "1 1 1 1 1" <~> "0.2 0.9")
@

It is assumed the pattern fits into a single cycle. This works well with
pattern literals, but not always with patterns defined elsewhere. In those cases
use @prr@ and provide desired pattern lengths:
@
let p = slow 2 $ "x x x"

d1 $ sound $ prr 0 (2,1) p "bd sn"
@
-}
preplace :: (Time, Time) -> Pattern a -> Pattern a -> Pattern a
preplace = preplaceWith $ flip const

prep = preplace

preplace1 :: Pattern a -> Pattern a -> Pattern a
preplace1 = prr 0 (1, 1)

preplaceWith :: (a -> b -> c) -> (Time, Time) -> Pattern a -> Pattern b -> Pattern c
preplaceWith f (blen, plen) = prrw f 0 (blen, plen)

prw = preplaceWith

preplaceWith1 :: (a -> b -> c) -> Pattern a -> Pattern b -> Pattern c
preplaceWith1 f = prrw f 0 (1, 1)

prw1 = preplaceWith1

(<~>) :: Pattern a -> Pattern a -> Pattern a
(<~>) = preplace (1, 1)

-- | @protate len rot p@ rotates pattern @p@ by @rot@ beats to the left.
-- @len@: length of the pattern, in cycles.
-- Example: @d1 $ every 4 (protate 2 (-1)) $ slow 2 $ sound "bd hh hh hh"@
protate :: Time -> Int -> Pattern a -> Pattern a
protate len rot p = prr rot (len, len) p p

prot = protate
prot1 = protate 1

{-| The @<<~@ operator rotates a unit pattern to the left, similar to @<~@,
but by events rather than linear time. The timing of the pattern remains constant:

@
d1 $ (1 <<~) $ sound "bd ~ sn hh"
-- will become
d1 $ sound "sn ~ hh bd"
@ -}

(<<~) :: Int -> Pattern a -> Pattern a
(<<~) = protate 1

(~>>) :: Int -> Pattern a -> Pattern a
(~>>) = (<<~) . (0-)

-- | @pequal cycles p1 p2@: quickly test if @p1@ and @p2@ are the same.
pequal :: Ord a => Time -> Pattern a -> Pattern a -> Bool
pequal cycles p1 p2 = (sort $ arc p1 (0, cycles)) == (sort $ arc p2 (0, cycles))

-- | @discretise n p@: 'samples' the pattern @p@ at a rate of @n@
-- events per cycle. Useful for turning a continuous pattern into a
-- discrete one.
discretise :: Time -> Pattern a -> Pattern a
discretise n p = density n $ (atom (id)) <*> p

-- | @randcat ps@: does a @slowcat@ on the list of patterns @ps@ but
-- randomises the order in which they are played.
randcat :: [Pattern a] -> Pattern a
randcat ps = spread' (<~) (discretise 1 $ ((%1) . fromIntegral) <$> irand (fromIntegral $ length ps)) (slowcat ps)

toMIDI :: Pattern String -> Pattern Int
toMIDI p = fromJust <$> (filterValues (isJust) (noteLookup <$> p))
  where
    noteLookup [] = Nothing
    noteLookup s | last s `elem` ['0' .. '9'] = elemIndex s names
                 | otherwise = noteLookup (s ++ "5")
    names = take 128 [(n ++ show o)
                     | o <- octaves,
                       n <- notes
                     ]
    notes = ["c","cs","d","ds","e","f","fs","g","gs","a","as","b"]
    octaves = [0 .. 10]

tom = toMIDI

fit :: Int -> [a] -> Pattern Int -> Pattern a
fit perCycle xs p = (xs !!!) <$> (Pattern $ \a -> map ((\e -> (mapThd' (+ (cyclePos perCycle e)) e))) (arc p a))
  where cyclePos perCycle e = perCycle * (floor $ eventStart e)

