module Tidal.CoreB where

import Criterion.Main
import Sound.Tidal.Core
import Sound.Tidal.Pattern
import Tidal.Inputs

_fastB :: [Benchmark]
_fastB =
  [ bgroup
      "_fast"
      [ bench "_fast < 0" $ whnf (_fast (-2)) pattApp2,
        bench "_fast > 0" $ whnf (_fast (toTime $ 10 ^ 6)) (cat catPattBig)
      ]
  ]

concatB :: [Benchmark]
concatB =
  [ bgroup
      "concat"
      [ bench "fastCat 10^3" $ whnf fastCat catPattSmall,
        bench "fastCat 10^4" $ whnf fastCat catPattMed,
        bench "fastCat 10^5" $ whnf fastCat catPattMedB,
        bench "fastCat 10^6" $ whnf fastCat catPattBig,
        bench "timeCat 10^5" $ whnf timeCat timeCatMed,
        bench "timeCat 10^6" $ whnf timeCat timeCatBig
      ]
  ]

fromListB :: [Benchmark]
fromListB =
  [ bgroup
      "fromList"
      [ bench "fromList" $ whnf fromList xs6,
        bench "fromList nf" $ nf fromList xs6,
        bench "fastFromList 10^3" $ whnf fastFromList xs3,
        bench "fastFromList 10^4" $ whnf fastFromList xs4,
        bench "fastFromList 10^5" $ whnf fastFromList xs5,
        bench "fastFromList 10^6" $ whnf fastFromList xs6,
        bench "fastFromList 10^6 nf" $ nf fastFromList xs6
      ]
  ]

appendB :: [Benchmark]
appendB =
  [ bgroup
      "append"
      [ bench "append" $ whnf (append pattApp1) pattApp2,
        bench "fastAppend" $ whnf (fastAppend pattApp1) pattApp2
      ]
  ]

stackB :: [Benchmark]
stackB =
  [ bgroup
      "stack"
      [ bench "overlay" $ whnf (overlay pattApp1) pattApp2,
        bench "stack" $ whnf stack catPattBig
      ]
  ]
