module Sound.Tidal.Config where


data Config = Config {cCtrlListen :: Bool,
                      cCtrlAddr :: String,
                      cCtrlPort :: Int,
                      cFrameTimespan :: Double,
                      cTempoAddr :: String,
                      cTempoPort :: Int,
                      cTempoClientPort :: Int,
                      cSendParts :: Bool,
                      cSkipTicks :: Int
                     }

defaultConfig :: Config
defaultConfig = Config {cCtrlListen = True,
                        cCtrlAddr ="127.0.0.1",
                        cCtrlPort = 6010,
                        cFrameTimespan = 1/20,
                        cTempoAddr = "127.0.0.1",
                        cTempoPort = 9160,
                        cTempoClientPort = 0, -- choose at random
                        cSendParts = False,
                        cSkipTicks = 10
                       }
