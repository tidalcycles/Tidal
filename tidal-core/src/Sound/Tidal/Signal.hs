module Sound.Tidal.Signal where

import           Data.Maybe           (fromMaybe, mapMaybe)
import           Sound.Tidal.Event
import           Sound.Tidal.Pattern
import           Sound.Tidal.Time
import           Sound.Tidal.TimeSpan
import           Sound.Tidal.Types

instance Monad Signal         where (>>=) = sigBindWith $ liftA2 sect
                                    return = pure
-- Define applicative from monad
instance Applicative Signal where
  pure v = Signal $ \q -> map (\arc -> Event mempty (Just $ timeToCycle $ aBegin arc) arc v) $ splitSpans q
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
  cat pats = splitQueries $ Signal $ \a -> query (_late (offset a) (pats !! mod (floor $ aBegin a) n)) a
    where offset arc = sam (aBegin arc) - sam (aBegin arc / toRational n)
          n = length pats
  timeCat tps = stack $ map (\(s,e,p) -> _compressSpan (Span (s/total) (e/total)) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Signal a)] -> [(Time, Time, Signal a)]
          arrange _ []            = []
          arrange t ((t',p):tps') = (t,t+t',p) : arrange (t+t') tps'
  stack pats = Signal $ \a -> concatMap (`query` a) pats
  _early t = withTime (subtract t) (+ t)
  rev pat = splitQueries $ Signal f
    where f a = eventWithSpan reflect <$> (query pat $ reflect a)
            where cyc = sam $ aBegin a
                  next_cyc = nextSam cyc
                  reflect (Span b e) = Span (cyc + (next_cyc - e)) (cyc + (next_cyc - b))
  toSignal = id
  withMetadata f pat = withEvents (map (\e -> e {eventMetadata = f $ eventMetadata e})) pat
  silence = Signal $ const []

-- instance Signalable (Signal a) a where toSig = id
-- instance Signalable a a where toSig = pure

-- | Split queries at sample boundaries. An internal function that
-- makes other functions easier to define, as events that cross cycle
-- boundaries don't need to be considered then.
splitQueries :: Signal a -> Signal a
splitQueries pat = Signal $ concatMap (query pat) . splitSpans

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
withEventSpan arcf = withEvent $ \e -> e {active = arcf $ active e,
                                          whole = arcf <$> whole e
                                         }

withQuery :: (Span -> Span) -> Signal a -> Signal a
withQuery arcf sig = Signal $ \arc -> query sig $ arcf arc

withQueryMaybe :: (Span -> Maybe Span) -> Signal a -> Signal a
withQueryMaybe qf pat = Signal $ \q -> fromMaybe [] $ qf q >>= Just . query pat

withQueryTime :: (Time -> Time) -> Signal a -> Signal a
withQueryTime timef = withQuery $ withSpanTime timef

-- Makes a signal bind, given a function of how to calculate the 'whole' timespan
sigBindWith :: (Maybe Span -> Maybe Span -> Maybe Span) -> Signal a -> (a -> Signal b) -> Signal b
sigBindWith chooseWhole pv f = Signal $ \q -> concatMap match $ query pv q
  where match event = map (withWhole event) $ query (f $ value event) (active event)
        withWhole event event' = event' {whole = chooseWhole (whole event) (whole event')}

-- | Like @join@, but cycles of the inner patterns are compressed to fit the
-- timearc of the outer whole (or the original query if it's a continuous pattern?)
-- TODO - what if a continuous pattern contains a discrete one, or vice-versa?
sigSqueezeJoin :: Signal (Signal a) -> Signal a
sigSqueezeJoin pp = pp {query = q}
  where q st = concatMap
          (\e@(Event m w p v) ->
             mapMaybe (munge m w p) $ query (_focusSpan (wholeOrActive e) v) p
          )
          (query pp st)
        munge oMetadata oWhole oPart (Event iMetadata iWhole iPart v) =
          do w' <- maybeSect <$> oWhole <*> iWhole
             p' <- maybeSect oPart iPart
             return (Event (iMetadata <> oMetadata) w' p' v)

_zoomSpan :: Span -> Signal a -> Signal a
_zoomSpan (Span s e) p = splitQueries $ withEventSpan (mapCycle ((/d) . subtract s)) $ withQuery (mapCycle ((+s) . (*d))) p
     where d = e-s

-- TODO - why is this function so long?
_fastGap :: Time -> Signal a -> Signal a
_fastGap factor pat = splitQueries $ withEvent ef $ withQueryMaybe qf pat
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

_compressSpan :: Span -> Signal a -> Signal a
_compressSpan (Span b e) pat | b > e || b > 1 || e > 1 || b < 0 || e < 0 = silence
                           | otherwise = _late b $ _fastGap (1/(e-b)) pat

-- | Similar to @fastCat@, but each signal is given a relative duration
sigTimeCat :: [(Time, Signal a)] -> Signal a
sigTimeCat tps = stack $ map (\(s,e,p) -> _compressSpan (Span (s/total) (e/total)) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Signal a)] -> [(Time, Time, Signal a)]
          arrange _ []            = []
          arrange t ((t',p):tps') = (t,t+t',p) : arrange (t+t') tps'

_focusSpan :: Span -> Signal a -> Signal a
_focusSpan (Span b e) pat = _late (cyclePos b) $ _fast (1/(e-b)) pat

