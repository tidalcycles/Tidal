import Sound.Tidal.Listener
import Sound.Tidal.Listener.Command
import Options.Applicative (execParser)

main :: IO ()
main = do
  config <- execParser conf
  listenWithConfig config
