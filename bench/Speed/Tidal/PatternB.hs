module Tidal.PatternB where

import Criterion.Main (Benchmark, bench, bgroup, nf, whnf)
import Sound.Tidal.Pattern
  ( ArcF (Arc),
    Time,
    hull,
    sect,
    subArc,
    withQueryArc,
    withQueryTime,
    withResultArc,
  )
import Tidal.Inputs (arcFunc, wqaBig, wqaMed)

arc1 :: ArcF Time
arc1 = Arc 3 5

arc2 :: ArcF Time
arc2 = Arc 4 6

arc3 :: ArcF Time
arc3 = Arc 0 1

arc4 :: ArcF Time
arc4 = Arc 1 2

withQueryTimeB :: [Benchmark]
withQueryTimeB =
  [ bgroup
      "withQueryTime"
      [ bench "wqt whnf" $ whnf withQueryTime (* 2),
        bench "wqt2 whnf" $ whnf withQueryTime (+ 1),
        bench "wqt nf" $ nf withQueryTime (* 2)
      ]
  ]

withResultArcB :: [Benchmark]
withResultArcB =
  [ bgroup
      "withResultArc"
      [ bench "wqa med" $ whnf (withResultArc arcFunc) wqaMed,
        bench "wqa big" $ whnf (withResultArc arcFunc) wqaBig
      ]
  ]

withQueryArcB :: [Benchmark]
withQueryArcB =
  [ bgroup
      "withQueryArc"
      [ bench "wqa med" $ whnf (withQueryArc arcFunc) wqaMed,
        bench "wqa big" $ whnf (withQueryArc arcFunc) wqaBig
      ]
  ]

subArcB :: [Benchmark]
subArcB =
  [ bgroup
      "subArc"
      [ bench "intersecting" $ whnf (subArc arc1) arc2,
        bench "non-intersecting" $ whnf (subArc arc3) arc4
      ]
  ]

sectB :: Benchmark
sectB = bench "sect" $ whnf (sect arc1) arc2

hullB :: Benchmark
hullB = bench "hull" $ whnf (hull arc1) arc2
