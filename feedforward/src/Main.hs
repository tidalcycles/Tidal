module Main where

import           Data.Semigroup          as S ((<>))  
import           Options.Applicative
import qualified Edit as E
import           Parameters

main :: IO ()
main = E.feedforward =<< execParser opts
  where
    opts = info (parameters <**> helper) fullDesc