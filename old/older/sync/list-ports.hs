import qualified Sound.ALSA.Sequencer.Client.Info as ClientInfo
import qualified Sound.ALSA.Sequencer.Port.Info as PortInfo
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Data.EnumSet as EnumSet
import Text.Printf (printf, )
import Control.Monad (liftM5, join, )

main :: IO ()
main = do
  putStrLn " Port    Client name                      Port name                Caps"
  SndSeq.withDefault SndSeq.Block $ \h ->
    ClientInfo.queryLoop_ (h :: SndSeq.T SndSeq.OutputMode) $ \cinfo -> do
      client <- ClientInfo.getClient cinfo
      PortInfo.queryLoop_ h client $ \pinfo -> do
        join $ liftM5 (printf "%3d:%-3d  %-32.32s %-24.24s %s\n")
          (fmap (\(Client.Cons p) -> p) $ PortInfo.getClient pinfo)
          (fmap (\(Port.Cons p) -> p) $ PortInfo.getPort pinfo)
          (ClientInfo.getName cinfo)
          (PortInfo.getName pinfo)
          (do
             caps <- PortInfo.getCapability pinfo
             let disp (cap, char) =
                    if EnumSet.disjoint caps cap then ' ' else char
             return $ map disp $
               (Port.capRead, 'r') :
               (Port.capSubsRead, 'R') :
               (Port.capWrite, 'w') :
               (Port.capSubsWrite, 'W') :
               [])
