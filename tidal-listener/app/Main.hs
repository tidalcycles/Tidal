import Options.Applicative (execParser)
import Sound.Tidal.Listener
import Sound.Tidal.Listener.Command

main :: IO ()
main = do
  config <- execParser conf
  listenWithConfig config
