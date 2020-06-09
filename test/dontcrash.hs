-- | test cases collected from some "Crash bugs"

{-# language OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

import Sound.Tidal.Context


main = do
  tidal <- startTidal (superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})
  let p = streamReplace tidal
      d1 = p 1 . (|< orbit 0)

  -- This will execute patterns-that-crash-tidal one after another,
  -- interspersed with a simple pattern.
  -- The test is whether we hear that simple pattern each time,
  -- indicating that the Tidal main loop is still usable.
  let go ps = forM_ (zip [0::Int ..] ps) $ \ (k,p) -> do
        let wait s = threadDelay $ s * 10^6
            simple = s "[bd*4, 808cy*8]"
        putStrLn $ "--- playing test pattern " ++ show k ++ " -----"
        d1 $ p      ; wait 2
        putStrLn $ "---------------- playing simple pattern"
        d1 $ simple ; wait 2

  go [ "cr"

       -- https://github.com/tidalcycles/Tidal/issues/606#issue-563234396
     , gain (unwrap $ fmap (["1", "0."]!!) $ "{0 0@7 0 1@7}%16") # s "harmor" # midichan 11

       -- https://github.com/tidalcycles/Tidal/issues/606#issuecomment-598776256
     , superimpose (hurry "<0.5 2?") $ sound "bd"

       -- https://github.com/tidalcycles/Tidal/issues/477#issue-411754641
     , let mkpat name pattern = (name,pattern)
           mkfx name fx = (name,fx)
           structure = cat [
             "kicks@8 [kicks,snares]@7 kicks:backrush"
             ,    "[kicks@3 [kicks@3 kicks(3,8,1):r]]@4 [kicks]@4 [kicks]@7 kicks:r"
             ]
           pats = [ mkpat "kicks" $ sometimes ghost $ s "bd(<4 5 3 6>,16,<0 1 0 3>)" ]
           fx = [ mkfx "r" (# speed "-1") ]
       in ur 16 structure pats fx
     ]
