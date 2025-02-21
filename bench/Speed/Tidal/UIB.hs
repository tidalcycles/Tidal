{-# LANGUAGE OverloadedStrings #-}

module Tidal.UIB where

import Criterion.Main
import Sound.Tidal.Context
import Tidal.Inputs

fixB :: [Benchmark]
fixB =
  [ bgroup
      "fix"
      [ bench "fix whnf" $ whnf (fix (fast 2) fixArg1) fixArg2,
        bench "fix nf" $ nf (fix (fast 2) fixArg1) fixArg2
      ]
  ]

euclidB :: [Benchmark]
euclidB =
  [ bgroup
      "euclid"
      [ bench "euclid" $ whnf (euclid (head ecA1) (head $ Prelude.drop 1 ecA1)) ecA2,
        bench "euclidFull" $ whnf (euclidFull (head ecA1) (head $ Prelude.drop 1 ecA1) ecA2) ecA2,
        bench "euclidBool" $ whnf (_euclidBool 1) 100000
      ]
  ]
