import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer.Connect as Connect
import Sound.Tidal.Tempo (Tempo, logicalTime, clocked, clockedTick, bps)
import System.Environment (getArgs, )
import Data.Maybe
import GHC.Word
import GHC.Int

import Sound.OSC.FD
import Sound.OSC.Datum
--import Sound.OpenSoundControl
--import Sound.OSC.FD
import System.IO
import Control.Concurrent

channel = Event.Channel 0

mykip = "192.168.178.135";
mykport = 57120

main :: IO ()
main = do --myk <- openUDP mykip mykport
          h <- SndSeq.openDefault SndSeq.Block
          Client.setName (h :: SndSeq.T SndSeq.OutputMode) "Tidal"
          c <- Client.getId h
          p <- Port.createSimple h "out"
               (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
          as <- getArgs
          let dev = fromMaybe "28:0" $ listToMaybe as
          conn <- Connect.createTo h p =<< Addr.parse h dev
          clockedTick 4 $ onTick h conn

wave n = drop i s ++ take i s
  where s = "¸.·´¯`·.´¯`·.¸¸.·´¯`·.¸<" ++ eye ++ ")))><"
        i = n `mod` (length s)
        eye | n `mod` 4 == 0 = "O"
            | otherwise = "º"

--onTick :: UDP -> Tempo -> Int -> IO ()
onTick h conn current ticks = 
  do putStr $ "tickmyk " ++ (show ticks) ++ " " ++ (wave ticks) ++ "\r"
     hFlush stdout
     --let m = Message "/sync" [int32 ticks, float ((bps current) * 60)]
     forkIO $ do threadDelay $ floor $ 0.179 * 1000000
                 Event.outputDirect h $ noteOn conn (fromIntegral $ ticks `mod` 128) 127
                 return ()
                 
                 --sendOSC myk m
     return ()

noteOn :: Connect.T -> Word8 -> Word8 -> Event.T
noteOn conn n v = 
  Event.forConnection conn 
  $ Event.NoteEv Event.NoteOn
  $ Event.simpleNote channel
                     (Event.Pitch (n))
                     (Event.Velocity v)
