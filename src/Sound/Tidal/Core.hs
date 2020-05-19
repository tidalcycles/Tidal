{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, BangPatterns #-}

module Sound.Tidal.Core where

import           Prelude hiding ((<*), (*>))

import           Data.Fixed (mod')
import qualified Data.Map.Strict as Map

import           Sound.Tidal.Pattern

-- ** Elemental patterns

-- | An empty pattern
silence :: Pattern a
silence = empty

-- | Takes a function from time to values, and turns it into a 'Pattern'.
sig :: (Time -> a) -> Pattern a
sig f = Pattern q
  where q (State (Arc s e) _)
          | s > e = []
          | otherwise = [Event (Context []) Nothing (Arc s e) (f (s+((e-s)/2)))]

-- | @sine@ returns a 'Pattern' of continuous 'Fractional' values following a
-- sinewave with frequency of one cycle, and amplitude from 0 to 1.
sine :: Fractional a => Pattern a
sine = sig $ \t -> (sin_rat ((pi :: Double) * 2 * fromRational t) + 1) / 2
  where sin_rat = fromRational . toRational . sin

-- | @cosine@ is a synonym for @0.25 ~> sine@.
cosine :: Fractional a => Pattern a
cosine = 0.25 `rotR` sine

-- | @saw@ is the equivalent of 'sine' for (ascending) sawtooth waves.
saw :: (Fractional a, Real a) => Pattern a
saw = sig $ \t -> mod' (fromRational t) 1

-- | @isaw@ is the equivalent of 'sine' for inverse (descending) sawtooth waves.
isaw :: (Fractional a, Real a) => Pattern a
isaw = (1-) <$> saw

-- | @tri@ is the equivalent of 'sine' for triangular waves.
tri :: (Fractional a, Real a) => Pattern a
tri = fastAppend saw isaw

-- | @square@ is the equivalent of 'sine' for square waves.
square :: (Fractional a) => Pattern a
square = sig $
       \t -> fromIntegral ((floor $ mod' (fromRational t :: Double) 1 * 2) :: Integer)

-- | @envL@ is a 'Pattern' of continuous 'Double' values, representing
-- a linear interpolation between 0 and 1 during the first cycle, then
-- staying constant at 1 for all following cycles. Possibly only
-- useful if you're using something like the retrig function defined
-- in tidal.el.
envL :: Pattern Double
envL = sig $ \t -> max 0 $ min (fromRational t) 1

-- | like 'envL' but reversed.
envLR :: Pattern Double
envLR = (1-) <$> envL

-- | 'Equal power' version of 'env', for gain-based transitions
envEq :: Pattern Double
envEq = sig $ \t -> sqrt (sin (pi/2 * max 0 (min (fromRational (1-t)) 1)))

-- | Equal power reversed
envEqR :: Pattern Double
envEqR = sig $ \t -> sqrt (cos (pi/2 * max 0 (min (fromRational (1-t)) 1)))

-- ** Pattern algebra

-- class for types that support a left-biased union
class Unionable a where
  union :: a -> a -> a

-- default union is just to take the left hand side..
instance Unionable a where
  union = const

instance {-# OVERLAPPING #-} Unionable ControlMap where
  union = Map.union

(|+|) :: (Applicative a, Num b) => a b -> a b -> a b
a |+| b = (+) <$> a <*> b
(|+ ) :: Num a => Pattern a -> Pattern a -> Pattern a
a |+  b = (+) <$> a <* b
( +|) :: Num a => Pattern a -> Pattern a -> Pattern a
a  +| b = (+) <$> a *> b

(|++|) :: Applicative a => a String -> a String -> a String
a |++| b = (++) <$> a <*> b
(|++ ) :: Pattern String -> Pattern String -> Pattern String
a |++  b = (++) <$> a <* b
( ++|) :: Pattern String -> Pattern String -> Pattern String
a  ++| b = (++) <$> a *> b

(|/|) :: (Applicative a, Fractional b) => a b -> a b -> a b
a |/| b = (/) <$> a <*> b
(|/ ) :: Fractional a => Pattern a -> Pattern a -> Pattern a
a |/  b = (/) <$> a <* b
( /|) :: Fractional a => Pattern a -> Pattern a -> Pattern a
a  /| b = (/) <$> a *> b

(|*|) :: (Applicative a, Num b) => a b -> a b -> a b
a |*| b = (*) <$> a <*> b
(|* ) :: Num a => Pattern a -> Pattern a -> Pattern a
a |*  b = (*) <$> a <* b
( *|) :: Num a => Pattern a -> Pattern a -> Pattern a
a  *| b = (*) <$> a *> b

(|-|) :: (Applicative a, Num b) => a b -> a b -> a b
a |-| b = (-) <$> a <*> b
(|- ) :: Num a => Pattern a -> Pattern a -> Pattern a
a |-  b = (-) <$> a <* b
( -|) :: Num a => Pattern a -> Pattern a -> Pattern a
a  -| b = (-) <$> a *> b

(|%|) :: (Applicative a, Real b) => a b -> a b -> a b
a |%| b = mod' <$> a <*> b
(|% ) :: Real a => Pattern a -> Pattern a -> Pattern a
a |%  b = mod' <$> a <* b
( %|) :: Real a => Pattern a -> Pattern a -> Pattern a
a  %| b = mod' <$> a *> b

(|**|) :: (Applicative a, Floating b) => a b -> a b -> a b
a |**| b = (**) <$> a <*> b
(|** ) :: Floating a => Pattern a -> Pattern a -> Pattern a
a |**  b = (**) <$> a <* b
( **|) :: Floating a => Pattern a -> Pattern a -> Pattern a
a  **| b = (**) <$> a *> b

(|>|) :: (Applicative a, Unionable b) => a b -> a b -> a b
a |>| b = flip union <$> a <*> b
(|> ) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a |>  b = flip union <$> a <* b
( >|) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a  >| b = flip union <$> a *> b

(|<|) :: (Applicative a, Unionable b) => a b -> a b -> a b
a |<| b = union <$> a <*> b
(|< ) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a |<  b = union <$> a <* b
( <|) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a  <| b = union <$> a *> b

-- Backward compatibility - structure from left, values from right.
(#) :: Unionable b => Pattern b -> Pattern b -> Pattern b
(#) = (|>)



-- ** Constructing patterns

-- | Turns a list of values into a pattern, playing one of them per cycle.
fromList :: [a] -> Pattern a
fromList = cat . map pure

-- | Turns a list of values into a pattern, playing all of them per cycle.
fastFromList :: [a] -> Pattern a
fastFromList = fastcat . map pure

-- | A synonym for 'fastFromList'
listToPat :: [a] -> Pattern a
listToPat = fastFromList

-- | 'fromMaybes; is similar to 'fromList', but allows values to
-- be optional using the 'Maybe' type, so that 'Nothing' results in
-- gaps in the pattern.
fromMaybes :: [Maybe a] -> Pattern a
fromMaybes = fastcat . map f
  where f Nothing = silence
        f (Just x) = pure x

-- | A pattern of whole numbers from 0 to the given number, in a single cycle.
run :: (Enum a, Num a) => Pattern a -> Pattern a
run = (>>= _run)

_run :: (Enum a, Num a) => a -> Pattern a
_run n = fastFromList [0 .. n-1]

-- | From @1@ for the first cycle, successively adds a number until it gets up to @n@
scan :: (Enum a, Num a) => Pattern a -> Pattern a
scan = (>>= _scan)

_scan :: (Enum a, Num a) => a -> Pattern a
_scan n = slowcat $ map _run [1 .. n]

-- ** Combining patterns

-- | Alternate between cycles of the two given patterns
append :: Pattern a -> Pattern a -> Pattern a
append a b = cat [a,b]

-- | Like 'append', but for a list of patterns. Interlaces them, playing the first cycle from each
-- in turn, then the second cycle from each, and so on.
cat :: [Pattern a] -> Pattern a
cat [] = silence
cat ps = Pattern $ q
  where n = length ps
        q st = concatMap (f st) $ arcCyclesZW (arc st)
        f st a = query (withResultTime (+offset) p) $ st {arc = Arc (subtract offset (start a)) (subtract offset (stop a))}
          where p = ps !! i
                cyc = (floor $ start a) :: Int
                i = cyc `mod` n
                offset = (fromIntegral $ cyc - ((cyc - i) `div` n)) :: Time

-- | Alias for 'cat'
slowCat :: [Pattern a] -> Pattern a
slowCat = cat
slowcat :: [Pattern a] -> Pattern a
slowcat = slowCat

-- | Alias for 'append'
slowAppend :: Pattern a -> Pattern a -> Pattern a
slowAppend = append
slowappend :: Pattern a -> Pattern a -> Pattern a
slowappend = append

-- | Like 'append', but twice as fast
fastAppend :: Pattern a -> Pattern a -> Pattern a
fastAppend a b = _fast 2 $ append a b
fastappend :: Pattern a -> Pattern a -> Pattern a
fastappend = fastAppend

-- | The same as 'cat', but speeds up the result by the number of
-- patterns there are, so the cycles from each are squashed to fit a
-- single cycle.
fastCat :: [Pattern a] -> Pattern a
fastCat ps = _fast (toTime $ length ps) $ cat ps

fastcat :: [Pattern a] -> Pattern a
fastcat = fastCat

-- | Similar to @fastCat@, but each pattern is given a relative duration
timeCat :: [(Time, Pattern a)] -> Pattern a
timeCat tps = stack $ map (\(s,e,p) -> compressArc (Arc (s/total) (e/total)) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Pattern a)] -> [(Time, Time, Pattern a)]
          arrange _ [] = []
          arrange t ((t',p):tps') = (t,t+t',p) : arrange (t+t') tps'

-- | 'overlay' combines two 'Pattern's into a new pattern, so that
-- their events are combined over time. 
overlay :: Pattern a -> Pattern a -> Pattern a
overlay !p !p' = Pattern $ \st -> query p st ++ query p' st

-- | An infix alias of @overlay@
(<>) :: Pattern a -> Pattern a -> Pattern a
(<>) = overlay

-- | 'stack' combines a list of 'Pattern's into a new pattern, so that
-- their events are combined over time.
stack :: [Pattern a] -> Pattern a
stack = foldr overlay silence



-- ** Manipulating time

-- | Shifts a pattern back in time by the given amount, expressed in cycles
(<~) :: Pattern Time -> Pattern a -> Pattern a
(<~) = tParam rotL

-- | Shifts a pattern forward in time by the given amount, expressed in cycles
(~>) :: Pattern Time -> Pattern a -> Pattern a
(~>) = tParam rotR

-- | Speed up a pattern by the given time pattern
fast :: Pattern Time -> Pattern a -> Pattern a
fast = tParam _fast

-- | Slow down a pattern by the factors in the given time pattern, 'squeezing'
-- the pattern to fit the slot given in the time pattern
fastSqueeze :: Pattern Time -> Pattern a -> Pattern a
fastSqueeze = tParamSqueeze _fast

-- | An alias for @fast@
density :: Pattern Time -> Pattern a -> Pattern a
density = fast

_fast :: Time -> Pattern a -> Pattern a
_fast r p | r == 0 = silence
          | r < 0 = rev $ _fast (negate r) p
          | otherwise = withResultTime (/ r) $ withQueryTime (* r) p

-- | Slow down a pattern by the given time pattern
slow :: Pattern Time -> Pattern a -> Pattern a
slow = tParam _slow
_slow :: Time -> Pattern a -> Pattern a
_slow 0 _ = silence
_slow r p = _fast (1/r) p

-- | Slow down a pattern by the factors in the given time pattern, 'squeezing'
-- the pattern to fit the slot given in the time pattern
slowSqueeze :: Pattern Time -> Pattern a -> Pattern a
slowSqueeze = tParamSqueeze _slow

-- | An alias for @slow@
sparsity :: Pattern Time -> Pattern a -> Pattern a
sparsity = slow

-- | @rev p@ returns @p@ with the event positions in each cycle
-- reversed (or mirrored).
rev :: Pattern a -> Pattern a
rev p =
  splitQueries $ p {
    query = \st -> map makeWholeAbsolute $
      mapParts (mirrorArc (midCycle $ arc st)) $
      map makeWholeRelative
      (query p st
        {arc = mirrorArc (midCycle $ arc st) (arc st)
        })
    }
  where makeWholeRelative :: Event a -> Event a
        makeWholeRelative (e@(Event {whole = Nothing})) = e
        makeWholeRelative (Event c (Just (Arc s e)) p'@(Arc s' e') v) =
          Event c (Just $ Arc (s'-s) (e-e')) p' v
        makeWholeAbsolute :: Event a -> Event a
        makeWholeAbsolute (e@(Event {whole = Nothing})) = e
        makeWholeAbsolute (Event c (Just (Arc s e)) p'@(Arc s' e') v) =
          Event c (Just $ Arc (s'-e) (e'+s)) p' v
        midCycle :: Arc -> Time
        midCycle (Arc s _) = sam s + 0.5
        mapParts :: (Arc -> Arc) -> [Event a] -> [Event a]
        mapParts f es = (\(Event c w p' v) -> Event c w (f p') v) <$> es
        -- | Returns the `mirror image' of a 'Arc' around the given point in time
        mirrorArc :: Time -> Arc -> Arc
        mirrorArc mid' (Arc s e) = Arc (mid' - (e-mid')) (mid'+(mid'-s))

{- | Plays a portion of a pattern, specified by a time arc (start and end time).
The new resulting pattern is played over the time period of the original pattern:

@
d1 $ zoom (0.25, 0.75) $ sound "bd*2 hh*3 [sn bd]*2 drum"
@

In the pattern above, `zoom` is used with an arc from 25% to 75%. It is equivalent to this pattern:

@
d1 $ sound "hh*3 [sn bd]*2"
@
-}
zoom :: (Time, Time) -> Pattern a -> Pattern a
zoom (s,e) = zoomArc (Arc s e)

zoomArc :: Arc -> Pattern a -> Pattern a
zoomArc (Arc s e) p = splitQueries $
  withResultArc (mapCycle ((/d) . subtract s)) $ withQueryArc (mapCycle ((+s) . (*d))) p
     where d = e-s

-- | @fastGap@ is similar to 'fast' but maintains its cyclic
-- alignment. For example, @fastGap 2 p@ would squash the events in
-- pattern @p@ into the first half of each cycle (and the second
-- halves would be empty). The factor should be at least 1
fastGap :: Pattern Time -> Pattern a -> Pattern a
fastGap = tParam _fastGap

-- | An alias for @fastGap@
densityGap :: Pattern Time -> Pattern a -> Pattern a
densityGap = fastGap

compress :: (Time,Time) -> Pattern a -> Pattern a
compress (s,e) = compressArc (Arc s e)

compressTo :: (Time,Time) -> Pattern a -> Pattern a
compressTo (s,e) = compressArcTo (Arc s e)

repeatCycles :: Int -> Pattern a -> Pattern a
repeatCycles n p = cat (replicate n p)

fastRepeatCycles :: Int -> Pattern a -> Pattern a
fastRepeatCycles n p = cat (replicate n p)

-- | * Higher order functions

-- | Functions which work on other functions (higher order functions)

-- | @every n f p@ applies the function @f@ to @p@, but only affects
-- every @n@ cycles.
every :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every tp f p = innerJoin $ (\t -> _every t f p) <$> tp

_every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_every 0 _ p = p
_every n f p = when ((== 0) . (`mod` n)) f p

-- | @every n o f'@ is like @every n f@ with an offset of @o@ cycles
every' :: Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every' np op f p = do { n <- np; o <- op; _every' n o f p }

_every' :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_every' n o = when ((== o) . (`mod` n))

-- | @foldEvery ns f p@ applies the function @f@ to @p@, and is applied for
-- each cycle in @ns@.
foldEvery :: [Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
foldEvery ns f p = foldr (`_every` f) p ns

{-|
Only `when` the given test function returns `True` the given pattern
transformation is applied. The test function will be called with the
current cycle as a number.

@
d1 $ when ((elem '4').show)
  (striate 4)
  $ sound "hh hc"
@

The above will only apply `striate 4` to the pattern if the current
cycle number contains the number 4. So the fourth cycle will be
striated and the fourteenth and so on. Expect lots of striates after
cycle number 399.
-}
when :: (Int -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
when test f p = splitQueries $ p {query = apply}
  where apply st | test (floor $ start $ arc st) = query (f p) st
                 | otherwise = query p st

-- | Like 'when', but works on continuous time values rather than cycle numbers.
whenT :: (Time -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
whenT test f p = splitQueries $ p {query = apply}
  where apply st | test (start $ arc st) = query (f p) st
                 | otherwise = query p st
