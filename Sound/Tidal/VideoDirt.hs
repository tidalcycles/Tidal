{-# LANGUAGE NoMonomorphismRestriction #-}
module VideoDirt where
import Sound.Tidal.Stream
import Sound.Tidal.Dirt

videoDirt :: Shape
videoDirt = Shape {   params = [ s_p,
                            n_p,
                            begin_p,
                            end_p,
                            speed_p,
                            xpos_p,
                            ypos_p,
                            zpos_p,
                            opacity_p,
                            blendmode_p
                          ],
                 cpsStamp = True,
                 latency = 0.3
                }

videoDirtSlang = dirtSlang { timestamp = BundleStamp, path = "/playVideo", namedParams = True }

videoDirtBackend = do
  s <- makeConnection "127.0.0.1" 7772 videoDirtSlang
  return $ Backend s (\_ _ _ -> return ())

videoDirtStream = do
  backend <- videoDirtBackend
  stream backend videoDirt

videoDirtState = do
  backend <- videoDirtBackend
  Sound.Tidal.Stream.state backend videoDirt

videoDirtSetters :: IO Time -> IO (ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
videoDirtSetters getNow = do ds <- videoDirtState
                        return (setter ds, transition getNow ds)
