  
module Sound.Tidal.VolcaKeys where

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer.Connect as Connect
import GHC.Word
import GHC.Int

import Sound.OSC.FD
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent.MVar
--import Visual
import Data.Hashable
import Data.Bits
import Data.Maybe
import System.Process
import Control.Concurrent

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse

channel = Event.Channel 0

keys :: OscShape
keys = OscShape {path = "/note",
                 params = [ I "note" Nothing,
                            F "dur" (Just (0.05)),
                            F "portamento" (Just (-1)),
                            F "expression" (Just (-1)),
                            F "voice" (Just (-1)),
                            F "octave" (Just (-1)),
                            F "detune" (Just (-1)),
                            F "vcoegint" (Just (-1)),
                            F "kcutoff" (Just (-1)),
                            F "vcfegint" (Just (-1)),
                            F "lforate" (Just (-1)),
                            F "lfopitchint" (Just (-1)),
                            F "lfocutoffint" (Just (-1)),
                            F "attack" (Just (-1)),
                            F "decay" (Just (-1)),
                            F "sustain" (Just (-1)),
                            F "dtime" (Just (-1)),
                            F "dfeedback" (Just (-1))
                          ],
                 cpsStamp = False,
                 timestamp = NoStamp,
                 latency = 0,
                 namedParams = False,
                 preamble = []
                }

keyStream = stream "127.0.0.1" 7303 keys

note         = makeI keys "note"
dur          = makeF keys "dur"
portamento   = makeF keys "portamento"
expression   = makeF keys "expression"
octave       = makeF keys "octave"
voice        = makeF keys "voice"
detune       = makeF keys "detune"
vcoegint     = makeF keys "vcoegint"
kcutoff      = makeF keys "kcutoff"
vcfegint     = makeF keys "vcfegint"
lforate      = makeF keys "lforate"
lfopitchint  = makeF keys "lfopitchint"
lfocutoffint = makeF keys "lfocutoffint"
attack       = makeF keys "attack"
decay        = makeF keys "decay"
sustain      = makeF keys "sustain"
dtime        = makeF keys "dtime"
dfeedback    = makeF keys "dfeedback"


keynames = map name (tail $ tail $ params keys)

keyproxy latency midiport = 
   do h <- SndSeq.openDefault SndSeq.Block
      Client.setName (h :: SndSeq.T SndSeq.OutputMode) "Tidal"
      c <- Client.getId h
      p <- Port.createSimple h "out"
           (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
      conn <- Connect.createTo h p =<< Addr.parse h midiport
      x <- udpServer "127.0.0.1" 7303
      forkIO $ loop h conn x
      return ()
         where loop h conn x = do m <- recvMessage x
                                  act h conn m
                                  loop h conn x
               act h conn (Just (Message "/note" (note:dur:ctrls))) = 
                   do -- print $ "Got note " ++ show note
                      let note' = (fromJust $ d_get note) :: Int
                          dur' = (fromJust $ d_get dur) :: Float
                          ctrls' = (map (fromJust . d_get) ctrls) :: [Float]
                      sendmidi latency h conn (fromIntegral note', dur') ctrls'
                      return ()


sendmidi latency h conn (note,dur) ctrls = 
  do forkIO $ do threadDelay latency
                 Event.outputDirect h $ noteOn conn note 60
                 threadDelay (floor $ 1000000 * dur)
                 Event.outputDirect h $ noteOff conn note
                 return ()
     let ctrls' = map (floor . (* 127)) ctrls
         ctrls'' = filter ((>=0) . snd) (zip keynames ctrls')
         --ctrls''' = map (\x -> (x, fromJust $ lookup x ctrls'')) usectrls
     --putStrLn $ show ctrls''
     sequence_ $ map (\(name, ctrl) -> Event.outputDirect h $ makeCtrl conn (ctrlN name ctrl)) ctrls''
     return ()

ctrlN "portamento" v    = (5, v)
ctrlN "expression" v    = (11, v)
ctrlN "voice" v         = (40, v)
ctrlN "octave" v        = (41, v)
ctrlN "detune" v        = (42, v)
ctrlN "vcoegint" v      = (43, v)
ctrlN "kcutoff" v        = (44, v)
ctrlN "vcfegint" v      = (45, v)
ctrlN "lforate" v       = (46, v)
ctrlN "lfopitchint" v   = (47, v)
ctrlN "lfocutoffint" v  = (48, v)
ctrlN "attack" v        = (49, v)
ctrlN "decay" v         = (50, v)
ctrlN "sustain" v       = (51, v)
ctrlN "dtime" v         = (52, v)
ctrlN "dfeedback" v     = (53, v)
ctrlN s _               = error $ "no match for " ++ s




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

makeCtrl :: Connect.T -> (Word32, GHC.Int.Int32) -> Event.T
makeCtrl conn (c, n) = 
  Event.forConnection conn 
  $ Event.CtrlEv Event.Controller $ Event.Ctrl 
                                    channel 
                                    (Event.Parameter c) 
                                    (Event.Value n)
