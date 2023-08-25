module Tidal.PatternB where

import           Criterion.Main          (Benchmark, bench, bgroup, nf, whnf)
import           Sound.Tidal.Arc         (hull, maybeSect, sect)
import           Sound.Tidal.Signal.Base (withEventArc, withQueryArc,
                                          withQueryTime)
import           Sound.Tidal.Types       (ArcF (Arc))
import           Tidal.Inputs            (arcFunc, wqaBig, wqaMed)

arc1 = Arc 3 5
arc2 = Arc 4 6
arc3 = Arc 0 1
arc4 = Arc 1 2

withQueryTimeB :: [Benchmark]
withQueryTimeB =
  [ bgroup "withQueryTime" [
      bench "wqt whnf" $ whnf withQueryTime (*2)
    , bench "wqt2 whnf" $ whnf withQueryTime (+1)
    , bench "wqt nf" $ nf withQueryTime (*2) ]
  ]

withEventArcB :: [Benchmark]
withEventArcB =
  [ bgroup "withEventArc" [
      bench "wqa med" $ whnf (withEventArc arcFunc) wqaMed
    , bench "wqa big" $ whnf (withEventArc arcFunc) wqaBig ]
  ]

withQueryArcB :: [Benchmark]
withQueryArcB =
  [ bgroup "withQueryArc" [
      bench "wqa med" $ whnf (withQueryArc arcFunc) wqaMed
    , bench "wqa big" $ whnf (withQueryArc arcFunc) wqaBig ]
  ]

maybeSectB :: [Benchmark]
maybeSectB =
  [ bgroup "maybeSect" [
      bench "intersecting" $ whnf (maybeSect arc1) arc2
    , bench "non-intersecting" $ whnf (maybeSect arc3) arc4 ]
  ]

sectB :: Benchmark
sectB = bench "sect" $ whnf (sect arc1) arc2

hullB :: Benchmark
hullB = bench "hull" $ whnf (hull arc1) arc2
