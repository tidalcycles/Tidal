import Sound.Tidal.Tempo (Tempo, logicalTime, clocked, clockedTick, bps)

import Sound.OSC.FD
import Sound.OSC.Datum
--import Sound.OpenSoundControl
--import Sound.OSC.FD
import System.IO

mykip = "10.0.1.10";
mykport = 4000

main :: IO ()
main = do myk <- openUDP mykip mykport
          clockedTick 4 $ onTick myk

wave n = drop i s ++ take i s
  where s = "¸.·´¯`·.´¯`·.¸¸.·´¯`·.¸<º)))><"
        i = n `mod` (length s)

onTick :: UDP -> Tempo -> Int -> IO ()
onTick myk current ticks = 
  do putStr $ "ticklc " ++ (show ticks) ++ " " ++ (wave ticks) ++ "\r"
     hFlush stdout
     let m = Message "/sync" [int32 ticks, float ((bps current) * 60)]
     sendOSC myk m
