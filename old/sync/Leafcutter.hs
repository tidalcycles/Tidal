
import Sound.Tidal.Tempo (State, Tempo, clocked, cps, ticks)
import Sound.Tidal.Config
import Control.Concurrent.MVar
import Control.Monad (when)

import Sound.OSC.FD
import Sound.OSC.Datum
--import Sound.OpenSoundControl
--import Sound.OSC.FD
import System.IO

tpb = 4

mykip = "10.0.1.10";
mykport = 4000
main :: IO ()
main = do myk <- openUDP mykip mykport
          tempoMV <- newEmptyMVar
          clocked defaultConfig tempoMV $ onTick myk tempoMV
          putStrLn "hmm."
          return ()

wave n = drop i s ++ take i s
  where s = "¸.·´¯`·.´¯`·.¸¸.·´¯`·.¸<º)))><"
        i = n `mod` (length s)

onTick :: UDP -> MVar Tempo -> State -> IO ()
onTick myk mtempo current = 
  do let t = ticks current
     when (t `mod` tpb == 0) $
       do tempo <- readMVar mtempo
          let b = t `div` tpb
              bpm = (cps tempo) * 60
          putStr $ show bpm ++ " : " ++ (show b) ++ " " ++ (wave b) ++ "\r"
          hFlush stdout
          let m = Message "/sync" [int32 b, float bpm]
          sendMessage myk m
