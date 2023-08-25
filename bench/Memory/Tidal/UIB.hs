module Tidal.UIB where 

import Weigh 
import Tidal.Inputs 
import Sound.Tidal.Context

fixB :: Weigh () 
fixB = 
  wgroup "fix weigh" $ do
    columns
    func "fix 1" (fix (fast 2) fixArg1) fixArg2

euclidB :: Weigh () 
euclidB = 
  wgroup "euclid" $ do 
    columns 
    func "euclid" (euclid (head ecA1) (head $ drop 1 ecA1)) ecA2
    func "euclidFull" (euclidFull (head ecA1) (head $ drop 1 ecA1) ecA2) ecA2
    func "euclidBool" (_euclidBool 1 :: Int -> Signal Bool) 100000
