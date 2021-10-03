module Sound.Tidal.Tidali where

import Sound.Tidal.Stream (Target(..))
import qualified Sound.Tidal.Context as T
import Sound.Tidal.Hint
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Sound.Tidal.Tempo as Tempo

data State = State {sIn :: MVar String,
                    sOut :: MVar Response,
                    sStream :: T.Stream
                   }

readBlock = do l <- getLine
               if l == ":{"
               then readBlock'
               else (return l)
  where readBlock' = do l <- getLine
                        if l == ":}"
                        then (return "")
                        else (fmap ((l ++) . ('\n':)) readBlock')

main :: IO ()
main = do -- start Haskell interpreter, with input and output mutable
          -- variables to communicate with it
          (mIn, mOut) <- startHint
          stream <-
            T.startStream T.defaultConfig [(T.superdirtTarget {oLatency = 0.1},
                                            [T.superdirtShape]
                                           )
                                          ]
          let st = State mIn mOut stream
          loopit st
   where
     loopit :: State -> IO ()
     loopit st =
       do block <- readBlock
          st' <- run st block
          loopit st'

-- TODO - use Chan or TChan for in/out channels instead of mvars directly?
startHint = do mIn <- newEmptyMVar
               mOut <- newEmptyMVar
               forkIO $ hintJob mIn mOut
               return (mIn, mOut)

getcps st = do tempo <- readMVar $ T.sTempoMV (sStream st)
               return (Tempo.cps tempo)

run :: State -> String -> IO State
run st code =
  do putMVar (sIn st) code
     r <- takeMVar (sOut st)
     respond r
     return st
       where respond (HintOK pat) = putStrLn $ show pat
             respond (HintError s) = putStrLn s
