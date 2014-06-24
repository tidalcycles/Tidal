import Sound.Tidal.Tempo (Tempo, logicalTime, clocked, clockedTick, bps)

import Sound.OSC.FD
import Sound.OSC.Datum
--import Sound.OpenSoundControl
--import Sound.OSC.FD
import System.IO

daveip = "192.168.0.3";
daveport = 4000
adeip = "10.0.0.3";
adeport = 1777;

tpb = 8

main :: IO ()
main = do dave <- openUDP daveip daveport
          clocked $ onTick dave

wave n = drop i s ++ take i s
  where s = "¸.·´¯`·.´¯`·.¸¸.·´¯`·.¸<º)))><"
        i = n `mod` (length s)

onTick :: UDP -> Tempo -> Int -> IO ()
onTick dave current ticks 
  | ticks `mod` 8 == 0 = 
    do putStr $ "tickdave " ++ (show ticks) ++ " " ++ (wave ticks) ++ "\r"
       hFlush stdout
       let m = Message "/sync" [int32 tpb, float ((bps current) * 60)]
       sendOSC dave m
  | otherwise = return ()

-- onTickAde :: UDP -> BpsChange -> Int -> IO ()
-- onTickAde ade current ticks = 
--     do let n = Message "/PureEvents/Beat" [Int ticks]
--        sendOSC ade n
