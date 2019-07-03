module Main where 

import Weigh 
import Tidal.UIB

main :: IO () 
main = 
  mainWith $ do 
    euclidB
    fixB
