module Tidal.UIB where

import Sound.Tidal.Context
  ( euclid,
    euclidFull,
    fast,
    fix,
    _euclidBool,
  )
import Tidal.Inputs (columns, ecA1, ecA2, fixArg1, fixArg2)
import Weigh (Weigh, func, wgroup)

fixB :: Weigh ()
fixB =
  wgroup "fix weigh" $ do
    columns
    func "fix 1" (fix (fast 2) fixArg1) fixArg2

euclidB :: Weigh ()
euclidB =
  wgroup "euclid" $ do
    columns
    func "euclid" (euclid (head ecA1) (ecA1 !! 1)) ecA2
    func "euclidFull" (euclidFull (head ecA1) (ecA1 !! 1) ecA2) ecA2
    func "euclidBool" (_euclidBool 1) 100000
