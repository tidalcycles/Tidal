module Sound.Tidal.Signal where

-- To get liftA2.. avoids import warning
import           Control.Applicative  (Applicative (..))
import           Prelude              hiding (Applicative (..), span)

import           Data.List            ((\\))
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust, fromMaybe, isJust, mapMaybe)
import           Sound.Tidal.Event
import           Sound.Tidal.Pattern
import           Sound.Tidal.Time
import           Sound.Tidal.TimeSpan
import           Sound.Tidal.Types

instance Monad Signal where
  (>>=) = sigBindWith $ liftA2 sect
  return = pure

-- Define applicative from monad
instance Applicative Signal where
  pure v = Signal $ \state -> map (\span -> Event mempty (Just $ timeToCycle $ aBegin span) span v) $ splitSpans $ sSpan state
  pf <*> px = pf >>= \f -> px >>= \x -> pure $ f x

instance Pattern Signal where
  -- We always work with signals as if they have a duration of 1
  -- cycle, even though successive cycles very often differ
  duration _ = 1
  withTime fa fb pat = withEventTime fa $ withQueryTime fb pat
  -- | Alternative binds/joins
  innerBind = sigBindWith $ flip const
  outerBind = sigBindWith const
  squeezeJoin = sigSqueezeJoin
  -- | Concatenate a list of signals, interleaving cycles.
  cat pats = splitQueries $ Signal $ \state -> query (_late (offset $ sSpan state) (pat $ sSpan state)) state
    where pat span = pats !! mod (floor $ aBegin span) n
          offset span = sam (aBegin span) - sam (aBegin span / toRational n)
          n = length pats
  timeCat tps = stack $ map (\(s,e,p) -> _compressSpan (Span (s/total) (e/total)) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Signal a)] -> [(Time, Time, Signal a)]
          arrange _ []            = []
          arrange t ((t',p):tps') = (t,t+t',p) : arrange (t+t') tps'
  stack pats = Signal $ \a -> concatMap (`query` a) pats
  _early t = withTime (subtract t) (+ t)
  rev pat = splitQueries $ Signal f
    where f state = eventWithSpan reflect <$> (query pat $ state {sSpan = reflect $ sSpan state})
            where cyc = sam $ aBegin $ sSpan state
                  next_cyc = nextSam cyc
                  reflect (Span b e) = Span (cyc + (next_cyc - e)) (cyc + (next_cyc - b))
  toSignal = id
  withMetadata f pat = withEvents (map (\e -> e {eventMetadata = f $ eventMetadata e})) pat
  silence = Signal $ const []
  _zoomSpan (Span s e) p = splitQueries
                           $ withEventSpan (mapCycle ((/d) . subtract s))
                           $ withQuerySpan (mapCycle ((+s) . (*d))) p
    where d = e-s

-- instance Signalable (Signal a) a where toSig = id
-- instance Signalable a a where toSig = pure

querySpan :: Signal a -> Span -> [Event a]
querySpan pat span = query pat $ State span Map.empty

-- | Split queries at sample boundaries. An internal function that
-- makes other functions easier to define, as events that cross cycle
-- boundaries don't need to be considered then.
splitQueries :: Signal a -> Signal a
splitQueries pat =
  Signal $ \state -> concatMap (\span -> query pat (state {sSpan = span}))
                     $ splitSpans $ sSpan state

filterEvents :: (Event a -> Bool) -> Signal a -> Signal a
filterEvents f pat = Signal $ \state -> filter f $ query pat state

filterValues :: (a -> Bool) -> Signal a -> Signal a
filterValues f = filterEvents (f . value)

filterJusts :: Signal (Maybe a) -> Signal a
filterJusts = fmap fromJust . filterValues isJust

-- | @withEvents f p@ returns a new @Signal@ with f applied to the
-- resulting list of events for each query function @f@.
withEvents :: ([Event a] -> [Event b]) -> Signal a -> Signal b
withEvents f p = p {query = f . query p}

-- | @withEvent f p@ returns a new @Signal@ with f applied to each
-- event queried function @f@.
withEvent :: (Event a -> Event b) -> Signal a -> Signal b
withEvent f = withEvents (map f)

withEventTime :: (Time -> Time) -> Signal a -> Signal a
withEventTime timef = withEvent $ \e -> e {active = withSpanTime timef $ active e,
                                           whole = withSpanTime timef <$> whole e
                                          }

withEventSpan :: (Span -> Span) -> Signal a -> Signal a
withEventSpan spanf = withEvent $ \e -> e {active = spanf $ active e,
                                           whole = spanf <$> whole e
                                          }

withQuery :: (State -> State) -> Signal a -> Signal a
withQuery statef sig = Signal $ \state -> query sig $ statef state

withQueryMaybe :: (State -> Maybe State) -> Signal a -> Signal a
withQueryMaybe statef sig = Signal $ \state -> fromMaybe [] $
                                               do state' <- statef state
                                                  return $ query sig state'

withQuerySpan :: (Span -> Span) -> Signal a -> Signal a
withQuerySpan spanf = withQuery (\state -> state {sSpan = spanf $ sSpan state})

withQuerySpanMaybe :: (Span -> Maybe Span) -> Signal a -> Signal a
withQuerySpanMaybe spanf = withQueryMaybe (\state -> do a <- spanf $ sSpan state
                                                        return $ state {sSpan = a}
                                          )

withQueryTime :: (Time -> Time) -> Signal a -> Signal a
withQueryTime timef = withQuerySpan $ withSpanTime timef

-- Makes a signal bind, given a function of how to calculate the 'whole' timespan
sigBindWith :: (Maybe Span -> Maybe Span -> Maybe Span) -> Signal a -> (a -> Signal b) -> Signal b
sigBindWith chooseWhole pv f = Signal $ \state -> concatMap (match (sControls state)) $ query pv state
  where match controls event = map (withWhole event)
                               $ query (f $ value event)
                               $ State {sSpan = active event,
                                        sControls = controls
                                       }
        withWhole event event' = event' {whole = chooseWhole (whole event) (whole event')}

-- | Like @join@, but cycles of the inner patterns are compressed to fit the
-- timespan of the outer whole (or the original query if it's a continuous pattern?)
-- TODO - what if a continuous pattern contains a discrete one, or vice-versa?
sigSqueezeJoin :: Signal (Signal a) -> Signal a
sigSqueezeJoin pp = pp {query = q}
  where q state = concatMap
          (\e@(Event m w p v) ->
             mapMaybe (munge m w p) $ query (_focusSpan (wholeOrActive e) v) $ state {sSpan = p}
          )
          (query pp state)
        munge oMetadata oWhole oPart (Event iMetadata iWhole iPart v) =
          do w' <- maybeSect <$> oWhole <*> iWhole
             p' <- maybeSect oPart iPart
             return (Event (iMetadata <> oMetadata) w' p' v)

{- | Plays a portion of a signal, specified by start and duration
The new resulting signal is played over the time period of the original signal:

@
d1 $ zoom 0.25 0.75 $ sound "bd*2 hh*3 [sn bd]*2 drum"
@

in the signal above, `zoom` is used with an span from 25% to 75%. It is equivalent to this signal:

@
d1 $ sound "hh*3 [sn bd]*2"
@
-}
zoom :: Signal Time -> Signal Time -> Signal a -> Signal a
zoom patStart patDur pat = innerJoin $ (\s d -> _zoomSpan (Span s (s+d)) pat) <$> patStart <*> patDur

-- TODO - why is this function so long?
_fastGap :: Time -> Signal a -> Signal a
_fastGap factor pat = splitQueries $ withEvent ef $ withQuerySpanMaybe qf pat
  -- A bit fiddly, to drop zero-width queries at the start of the next cycle
  where qf (Span b e) | bpos < 1 = Just $ Span (cyc + bpos) (cyc + epos)
                      | otherwise = Nothing
          where cyc = sam b
                bpos = min 1 $ (b - cyc) * factor
                epos = min 1 $ (e - cyc) * factor
        -- Also fiddly, to maintain the right 'whole' relative to the part
        ef ev = ev {whole = w', active = a'}
          where a = active ev
                b = aBegin a
                e = aEnd a
                a' = Span (cyc + bpos) (cyc + epos)
                  where cyc = sam b
                        bpos = min 1 $ (b - cyc) / factor
                        epos = min 1 $ (e - cyc) / factor
                w' = do w <- whole ev
                        let b' = aBegin a' - ((b - aBegin w) / factor)
                            e' = aEnd a' + ((aEnd w - e) / factor)
                        return $ Span b' e'

-- | Like @fast@, but only plays one cycle of the original signal
-- once per cycle, leaving a gap at the end
fastGap :: Signal Time -> Signal a -> Signal a
fastGap = patternify_p_n _fastGap

_compressSpan :: Span -> Signal a -> Signal a
_compressSpan (Span b e) pat | b > e || b > 1 || e > 1 || b < 0 || e < 0 = silence
                           | otherwise = _late b $ _fastGap (1/(e-b)) pat

-- | Like @fastGap@, but takes the start and duration of the span to compress the cycle into.
compress :: Signal Time -> Signal Time -> Signal a -> Signal a
compress patStart patDur pat = innerJoin $ (\s d -> _compressSpan (Span s (s+d)) pat) <$> patStart <*> patDur

-- | Similar to @fastCat@, but each signal is given a relative duration
sigTimeCat :: [(Time, Signal a)] -> Signal a
sigTimeCat tps = stack $ map (\(s,e,p) -> _compressSpan (Span (s/total) (e/total)) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Signal a)] -> [(Time, Time, Signal a)]
          arrange _ []            = []
          arrange t ((t',p):tps') = (t,t+t',p) : arrange (t+t') tps'

{-|
Only `when` the given test function returns `True` the given signal
transformation is applied. The test function will be called with the
current cycle as a number.

@
d1 $ whenT ((elem '4').show)
  (striate 4)
  $ sound "hh hc"
@

The above will only apply `striate 4` to the signal if the current
cycle number contains the number 4. So the fourth cycle will be
striated and the fourteenth and so on. Expect lots of striates after
cycle number 399.
-}
whenT :: (Int -> Bool) -> (Signal a -> Signal a) ->  Signal a -> Signal a
whenT test f p = splitQueries $ p {query = apply}
  where apply st | test (floor $ aBegin $ sSpan st) = query (f p) st
                 | otherwise = query p st

_focusSpan :: Span -> Signal a -> Signal a
_focusSpan (Span b e) pat = _late (cyclePos b) $ _fast (1/(e-b)) pat

-- | Like @compress@, but doesn't leave a gap and can 'focus' on any span (not just within a cycle)
focus :: Signal Time -> Signal Time -> Signal a -> Signal a
focus patStart patDur pat = innerJoin $ (\s d -> _focusSpan (Span s (s+d)) pat) <$> patStart <*> patDur

--- functions relating to chords/patterns of lists

_sameDur :: Event a -> Event a -> Bool
_sameDur e1 e2 = (whole e1 == whole e2) && (active e1 == active e2)

_groupEventsBy :: Eq a => (Event a -> Event a -> Bool) -> [Event a] -> [[Event a]]
_groupEventsBy _ [] = []
_groupEventsBy f (e:es) = eqs : _groupEventsBy f (es \\ eqs)
  where eqs = e:[x | x <- es, f e x]

-- assumes that all events in the list have same whole/active
_collectEvent :: [Event a] -> Maybe (Event [a])
_collectEvent [] = Nothing
_collectEvent es@(e:_) = Just $ e {eventMetadata = mconcat $ map eventMetadata es,
                                   value = map value es
                                  }

_collectEventsBy :: Eq a => (Event a -> Event a -> Bool) -> [Event a] -> [Event [a]]
_collectEventsBy f es = remNo $ map _collectEvent (_groupEventsBy f es)
  where
    remNo []            = []
    remNo (Nothing:cs)  = remNo cs
    remNo ((Just c):cs) = c : remNo cs

-- | collects all events satisfying the same constraint into a list
_collectBy :: Eq a => (Event a -> Event a -> Bool) -> Signal a -> Signal [a]
_collectBy f = withEvents (_collectEventsBy f)

-- TODO - define collect for sequences as well.

-- | collects all events occuring at the exact same time into a
-- list. See also 'uncollect' defined in the Pattern module.
collect :: Eq a => Signal a -> Signal [a]
collect = _collectBy _sameDur

