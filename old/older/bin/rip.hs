--import Sound.Tidal.Context
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer.Connect as Connect
import GHC.Word
import GHC.Int

import System.Cmd
import Control.Concurrent
import Text.Printf

channel = Event.Channel 0
notes = [12 .. 100]
time = 3

midiport = "24:0"

main =
   do h <- SndSeq.openDefault SndSeq.Block
      Client.setName (h :: SndSeq.T SndSeq.OutputMode) "rip"
      c <- Client.getId h
      p <- Port.createSimple h "out"
           (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
      conn <- Connect.createTo h p =<< Addr.parse h midiport
      sequence_ $ map (play h conn) notes
      return ()

play h conn n =
  do let tmpfn = printf "tmp-%03d.wav" n
         fn = printf "note-%03d.wav" n
     forkIO $ do rawSystem "ecasound" ["-t:" ++ (show time), "-i", "jack,system", "-o", tmpfn]
                 return ()
     threadDelay 500000
     Event.outputDirect h $ noteOn conn n 80
     forkIO $ do threadDelay 50000
                 Event.outputDirect h $ noteOff conn n
                 return ()
     threadDelay $ 1000000 * time
     forkIO $ do rawSystem "sox" [tmpfn, fn, "silence", "1", "0", "-55d", "reverse", "silence", "1", "0", "-55d", "reverse"]
                 rawSystem "rm" [tmpfn]
                 return ()
     return ()
     
     
noteOn :: Connect.T -> Word8 -> Word8 -> Event.T
noteOn conn val vel = 
  Event.forConnection conn 
  $ Event.NoteEv Event.NoteOn
  $ Event.simpleNote channel
                     (Event.Pitch (val))
                     (Event.Velocity vel)

noteOff :: Connect.T -> Word8 -> Event.T
noteOff conn val = 
  Event.forConnection conn 
  $ Event.NoteEv Event.NoteOff
  $ Event.simpleNote channel
                     (Event.Pitch (val))
                     (Event.normalVelocity)
