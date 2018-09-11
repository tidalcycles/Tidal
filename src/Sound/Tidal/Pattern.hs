module Sound.Tidal.Pattern where

import Data.Ratio
import Control.Applicative
import Data.Maybe (mapMaybe)
import Data.Fixed (mod')
import Data.Maybe (isJust, fromJust, catMaybes)

import Sound.Tidal.Utils

------------------------------------------------------------------------
-- Types

-- | Time is rational
type Time = Rational

-- | A time span (start and end)
type Span = (Time, Time)

-- | The second timespan (the part) should be equal to or fit inside the
-- first one (the whole that it's a part of)
type Part = (Span, Span)

-- | An event is a value that's active during a timespan
type Event a = (Part, a)

-- | A function that represents events taking place over time
type Query a = (Span -> [Event a])

-- | A datatype that's basically a query. At least for now.
data Pattern a = Pattern {query :: Query a}

------------------------------------------------------------------------
-- Instances

-- | Repeat the given value once per cycle, forever
atom v = Pattern $ \(s,e) -> map (\(s',e') -> (constrain (s,e) (s',e'),v)) $ cycleSpansInSpan (s,e)
    where constrain (s,e) (s',e') = ((s',e'), (max s s', min e e'))

instance Functor Pattern where
  -- | apply a function to all the values in a pattern
  fmap f = Pattern . (fmap (fmap (fmap f))) . query

instance Applicative Pattern where
  pure = atom

  -- for the part of each event in pf
  -- - get matching events px matching the span
  -- - for both whole and part, take the intersection of pf and px
  pf <*> px = Pattern f
    where f span = catMaybes $ concat $ map match $ query pf span
            where
              match fe@((fWhole, fPart), f) = map
                                              (\((xWhole, xPart),x) -> do w <- subSpan fWhole xWhole
                                                                          p <- subSpan fPart xPart
                                                                          return ((w,p),f x)
                                              )
                                              (query px fPart)

-- | Like <*>, but the structure comes from the left
(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
pf <* px = Pattern $ \a -> concatMap applyX $ query pf a
  where 
        applyX event@(((ws,_),_),f) =
          map (\(_,x) -> (fst event, f x)) $ query px (ws,ws)

-- | Like <*>, but the structure comes from the right
(*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
pf *> px = Pattern $ \a -> concatMap applyToF $ query px a
  where 
        applyToF event@(((ws,_),_),x) =
          map (\(_,f) -> (fst event, f x)) $ query pf (ws,ws)

instance Monad Pattern where
  return = atom
  p >>= f = joinPattern (f <$> p)

-- | Turns a pattern of patterns into a single pattern.
-- formerly known as unwrap
--
-- 1/ For query @span@, get the events from the outer pattern @pp@
-- 2/ Query the inner pattern using the 'part' of the outer
-- 3/ For each inner event, set the whole and part to be the intersection
--    of the outer whole and part, respectively
-- 4/ Concatenate all the events together (discarding wholes/parts that didn't intersect)

joinPattern :: Pattern (Pattern a) -> Pattern a
joinPattern pp = Pattern f
  where f span = concatMap (\((whole, part), p) -> catMaybes $ map (munge whole part) $ query p part) (query pp span)
        munge oWhole oPart ((iWhole, iPart),v) = do w <- subSpan oWhole iWhole
                                                    p <- subSpan oPart iPart
                                                    return ((w,p),v)

------------------------------------------------------------------------
-- Internal functions

-- | Get the timespan of an event's 'whole'
eventWhole :: Event a -> Span
eventWhole = fst . fst

-- | Get the timespan of an event's 'part'
eventPart :: Event a -> Span
eventPart = snd . fst

-- | Splits the given @Span@ into a list of @Span@s, at cycle boundaries.
spanCycles :: Span -> [Span]
spanCycles (s,e) | s >= e = []
                 | sam s == sam e = [(s,e)]
                 | otherwise = (s, nextSam s) : (spanCycles (nextSam s, e))

-- | Similar to @mapSpan@ but time is relative to the cycle (i.e. the
-- sam of the start of the arc)
mapCycle :: (Time -> Time) -> Span -> Span
mapCycle f (s,e) = (sam' + (f $ s - sam'), sam' + (f $ e - sam'))
         where sam' = sam s

-- | Splits queries that span cycles. For example `query p (0.5, 1.5)` would be
-- turned into two queries, `(0.5,1)` and `(1,1.5)`, and the results
-- combined. Being able to assume queries don't span cycles often
-- makes transformations easier to specify.
splitQueries :: Pattern a -> Pattern a
splitQueries p = Pattern $ \a -> concatMap (query p) $ spanCycles a

-- | The 'sam' (start of cycle) for the given time value
sam :: Time -> Time
sam = fromIntegral . floor

-- | Turns a number into a (rational) time value. An alias for @toRational@.
toTime :: Real a => a -> Rational
toTime = toRational

-- | The end point of the current cycle (and starting point of the next cycle)
nextSam :: Time -> Time
nextSam = (1+) . sam

-- | The position of a time value relative to the start of its cycle.
cyclePos :: Time -> Time
cyclePos t = t - sam t

-- | @subSpan i j@ is the timespan that is the intersection of @i@ and @j@.
subSpan :: Span -> Span -> Maybe Span
subSpan (s, e) (s',e') | s'' < e'' = Just (s'', e'')
                       | otherwise = Nothing
  where s'' = max s s'
        e'' = min e e'

-- | The span of the whole cycle that the given time value falls within
timeToCycleSpan :: Time -> Span
timeToCycleSpan t = (sam t, (sam t) + 1)

-- | A list of cycle numbers which are included in the given span
cyclesInSpan :: Integral a => Span -> [a]
cyclesInSpan (s,e) | s > e = []
                   | s == e = [floor s]
                   | otherwise = [floor s .. (ceiling e)-1]

-- | A list of spans of the whole cycles which are included in the given span
cycleSpansInSpan :: Span -> [Span]
cycleSpansInSpan = map (timeToCycleSpan . toTime) . cyclesInSpan

-- | Apply a function to the timespans (both whole and parts) of the result
withResultSpan :: (Span -> Span) -> Pattern a -> Pattern a
withResultSpan f p = Pattern $ \a -> map (mapFst (mapBoth f)) $ query p a

-- | Apply a function to the time (both start and end of the timespans
-- of both whole and parts) of the result
withResultTime :: (Time -> Time) -> Pattern a -> Pattern a
withResultTime = withResultSpan . mapBoth

-- | Apply a function to the timespan of the query
withQuerySpan :: (Span -> Span) -> Pattern a -> Pattern a
withQuerySpan f p = Pattern $ \a -> query p (f a)

-- | Apply a function to the time (both start and end) of the query
withQueryTime :: (Time -> Time) -> Pattern a -> Pattern a
withQueryTime = withQuerySpan . mapBoth

------------------------------------------------------------------------
-- UI

-- Elemental patterns

-- | An empty pattern
silence :: Pattern a
silence = Pattern $ const []

-- | Takes a function from time to values, and turns it into a @Pattern@.
sig :: (Time -> a) -> Pattern a
sig f = Pattern f'
  where f' (s,e) | s > e = []
                 -- experiment - what if all signals have a 'whole' starting at -1? So no onsets..
                 | otherwise = [(((-1,e), (s,e)), f s)]

-- | @sine@ returns a @Pattern@ of continuous @Fractional@ values following a
-- sinewave with frequency of one cycle, and amplitude from 0 to 1.
sine :: Fractional a => Pattern a
sine = sig $ \t -> ((sin_rat $ pi * 2 * (fromRational t)) + 1) / 2
  where sin_rat = fromRational . toRational . sin

-- | @cosine@ is a synonym for @0.25 ~> sine@.
cosine :: Fractional a => Pattern a
cosine = 0.25 `rotR` sine

-- | @saw@ is the equivalent of @sine@ for (ascending) sawtooth waves.
saw :: (Fractional a, Real a) => Pattern a
saw = sig $ \t -> mod' (fromRational t) 1

-- | @tri@ is the equivalent of @sine@ for triangular waves.
tri :: (Fractional a, Real a) => Pattern a
tri = append saw (rev saw)

-- | @square@ is the equivalent of @sine@ for square waves.
square :: (Fractional a, Real a) => Pattern a
square = sig $
         \t -> fromIntegral $ ((floor $ (mod' (fromRational t :: Double) 1) * 2) :: Integer)

-- | @envL@ is a @Pattern@ of continuous @Double@ values, representing
-- a linear interpolation between 0 and 1 during the first cycle, then
-- staying constant at 1 for all following cycles. Possibly only
-- useful if you're using something like the retrig function defined
-- in tidal.el.
envL :: Pattern Double
envL = sig $ \t -> max 0 $ min (fromRational t) 1

-- | like @envL@ but reversed.
envLR :: Pattern Double
envLR = (1-) <$> envL

-- | 'Equal power' version of @env@, for gain-based transitions
envEq :: Pattern Double
envEq = sig $ \t -> sqrt (sin (pi/2 * (max 0 $ min (fromRational (1-t)) 1)))

-- | Equal power reversed
envEqR :: Pattern Double
envEqR = sig $ \t -> sqrt (cos (pi/2 * (max 0 $ min (fromRational (1-t)) 1)))

-- Simple ways to combine patterns

-- | Alternate between cycles of the two given patterns
append a b = cat [a,b]

-- | Like @append@, but for a list of patterns. Interlaces them, playing the first cycle from each
-- in turn, then the second cycle from each, and so on.
cat :: [Pattern a] -> Pattern a
cat [] = silence
cat ps = Pattern f
  where l = length ps
        f a = concatMap f' $ spanCycles a
        f' a = query (withResultTime (+offset) p) $  mapBoth (subtract offset) a
          where p = ps !! n
                r = (floor $ fst a) :: Int
                n = r `mod` l
                offset = (fromIntegral $ r - ((r - n) `div` l)) :: Time

-- | Alias for @cat@
slowCat = cat
slowcat = slowCat

-- | Alias for @append@
slowAppend = append

-- | Like @append@, but twice as fast
fastAppend a b = _fast 2 $ append a b

-- | The same as @cat@, but speeds up the result by the number of
-- patterns there are, so the cycles from each are squashed to fit a
-- single cycle.
fastCat :: [Pattern a] -> Pattern a
fastCat ps = _fast (toTime $ length ps) $ cat ps
fastcat = fastCat

-- Ways to construct patterns

-- | Turns a list of values into a pattern, playing through them once per cycle.
fromList :: [a] -> Pattern a
fromList = fastCat . map atom

-- | A synonym for @fromList@
listToPat = fromList

-- | @fromMaybes@ is similar to @fromList@, but allows values to
-- be optional using the @Maybe@ type, so that @Nothing@ results in
-- gaps in the pattern.
fromMaybes :: [Maybe a] -> Pattern a
fromMaybes = fastcat . map f
  where f Nothing = silence
        f (Just x) = atom x

-- | A pattern of whole numbers from 0 to the given number, in a single cycle.
run = (>>= _run)
_run :: (Enum a, Num a) => a -> Pattern a
_run n = fromList [0 .. n-1]

-- | From @1@ for the first cycle, successively adds a number until it gets up to @n@
scan = (>>= _scan)
_scan :: (Enum a, Num a) => a -> Pattern a
_scan n = slowcat $ map _run [1 .. n]

-- Functions for manipulating time

-- | Shifts a pattern back in time by the given amount, expressed in cycles
rotL :: Time -> Pattern a -> Pattern a
rotL t p = withResultTime (subtract t) $ withQueryTime (+ t) p

-- | Infix alias for @rotL@
(<~) = temporalParam rotL

-- | Shifts a pattern forward in time by the given amount, expressed in cycles
rotR :: Time -> Pattern a -> Pattern a
rotR t = rotL (0-t)

-- | Infix alias for @rotR@
(~>) = temporalParam rotR

-- | Speed up a pattern by the given factor
fast tp = temporalParam _fast
_fast :: Time -> Pattern a -> Pattern a
_fast r p | r == 0 = silence
          | r < 0 = rev $ _fast (0-r) p
          | otherwise = withResultTime (/ r) $ withQueryTime (* r) p

-- | Slow down a pattern by the given factor
slow = temporalParam _slow
_slow :: Time -> Pattern a -> Pattern a
_slow r p = _fast (1/r) p

-- | @rev p@ returns @p@ with the event positions in each cycle
-- reversed (or mirrored).
rev :: Pattern a -> Pattern a
rev p = splitQueries $ Pattern $ \a -> map makeWholeAbsolute $ mapParts (mirrorSpan (mid a)) $ map makeWholeRelative (query p (mirrorSpan (mid a) a))
  where makeWholeRelative (((s,e), part@(s',e')), v) = (((s'-s, e-e'), part), v)
        makeWholeAbsolute (((s,e), part@(s',e')), v) = (((s'-e, e'+s), part), v)
        mid (s,_) = (sam s) + 0.5
        mapParts f es = map (mapFst (mapSnd f)) es
        -- | Returns the `mirror image' of a @Span@ around the given point in time
        mirrorSpan :: Time -> Span -> Span
        mirrorSpan mid (s, e) = (mid - (e-mid), mid+(mid-s))

{- | Plays a portion of a pattern, specified by a time span (start and end time).
The new resulting pattern is played over the time period of the original pattern:

@
d1 $ zoom (0.25, 0.75) $ sound "bd*2 hh*3 [sn bd]*2 drum"
@

In the pattern above, `zoom` is used with an arc from 25% to 75%. It is equivalent to this pattern:

@
d1 $ sound "hh*3 [sn bd]*2"
@
-}
zoom :: Span -> Pattern a -> Pattern a
zoom (s,e) p = splitQueries $ withResultSpan (mapCycle ((/d) . (subtract s))) $ withQuerySpan (mapCycle ((+s) . (*d))) p
     where d = e-s

-- | @fastGap@ is similar to @fast@ but maintains its cyclic
-- alignment. For example, @fastGap 2 p@ would squash the events in
-- pattern @p@ into the first half of each cycle (and the second
-- halves would be empty). The factor should be at least 1
fastGap = temporalParam _fastGap
_fastGap :: Time -> Pattern a -> Pattern a
_fastGap 0 _ = silence
_fastGap r p = splitQueries $ withResultSpan (\(s,e) -> (sam s + ((s - sam s)/r'), (sam s + ((e - sam s)/r')))) $ Pattern (\a -> query p $ mapBoth (\t -> sam t + (min 1 (r' * cyclePos t))) a)
  where r' = max r 1

compress :: Span -> Pattern a -> Pattern a
compress (s,e) p | s >= e = silence
                 | s > 1 || e > 1 = silence
                 | s < 0 || e < 0 = silence
                 | otherwise = s `rotR` _fastGap (1/(e-s)) p

-- | Event filters

-- | Remove events from patterns that to not meet the given test
filterValues :: (a -> Bool) -> Pattern a -> Pattern a
filterValues f (Pattern x) = Pattern $ (filter (f . snd)) . x

-- | Turns a pattern of @Maybe@ values in to a pattern of values,
-- dropping the events of @Nothing@.
filterJust :: Pattern (Maybe a) -> Pattern a
filterJust p = fromJust <$> (filterValues (isJust) p)

-- Temporal parameter helpers

temporalParam :: (a -> Pattern b -> Pattern c) -> (Pattern a -> Pattern b -> Pattern c)
temporalParam f tv p = joinPattern $ (`f` p) <$> tv

temporalParam2 :: (a -> b -> Pattern c -> Pattern d) -> (Pattern a -> Pattern b -> Pattern c -> Pattern d)
temporalParam2 f a b p = joinPattern $ (\x y -> f x y p) <$> a <*> b

temporalParam3 :: (a -> b -> c -> Pattern d -> Pattern e) -> (Pattern a -> Pattern b -> Pattern c -> Pattern d -> Pattern e)
temporalParam3 f a b c p = joinPattern $ (\x y z -> f x y z p) <$> a <*> b <*> c

--eoff :: Int -> Int -> Integer -> Pattern a -> Pattern a
--eoff n k s p = ((s%(fromIntegral k)) `rotL`) (_e n k p)
   -- TPat_ShiftL (s%(fromIntegral k)) (TPat_E n k p)


-- TODO

-- spread and friends

