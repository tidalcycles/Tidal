import Sound.Tidal.Tempo (Tempo, logicalTime, clocked, clockedTick, bps)

import Sound.OSC.FD
import Sound.OSC.Datum
--import Sound.OpenSoundControl
--import Sound.OSC.FD
import System.IO
import Control.Concurrent

mykip = "192.168.178.135";
mykport = 57120

main :: IO ()
main = do myk <- openUDP mykip mykport
          clockedTick 2 $ onTick myk

wave n = drop i s ++ take i s
  where s = "¸.·´¯`·.´¯`·.¸¸.·´¯`·.¸<º)))><"
        i = n `mod` (length s)

onTick :: UDP -> Tempo -> Int -> IO ()
onTick myk current ticks = 
  do putStr $ "tickmyk " ++ (show ticks) ++ " " ++ (wave ticks) ++ "\r"
     hFlush stdout
     let m = Message "/sync" [int32 ticks, float ((bps current) * 60)]
     forkIO $ do threadDelay $ floor $ 0.075 * 1000000
                 sendOSC myk m
     return ()
