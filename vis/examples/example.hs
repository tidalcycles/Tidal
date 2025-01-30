{-# LANGUAGE OverloadedStrings #-}

import Data.Colour
import Sound.Tidal.Context
import Sound.Tidal.Vis

render :: [Pattern ColourD] -> IO ()
render xs = mapM_ (\(n, p) -> vPDF (show n ++ ".pdf") (300, 100) p) $ zip [0 ..] xs

main = do
  render [a, b, c, d, e, f, g]
  return ()

a = density 16 $ every 2 rev $ every 3 (superimpose (iter 4)) $ rev "[black blue darkblue, grey lightblue]"

b = flip darken <$> "[black blue orange, red green]*16" <*> sinewave1

c =
  density 10 $
    flip darken
      <$> "[black blue, grey ~ navy, cornflowerblue blue]*2"
      <*> (slow 5 $ (*) <$> sinewave1 <*> (slow 2 triwave1))

d =
  every 2 rev $
    density 10 $
      ( blend'
          <$> "blue navy"
          <*> "orange [red, orange, purple]"
          <*> (slow 6 $ sinewave1)
      )
  where
    blend' a b c = blend c a b

e =
  density 32 $
    ( flip over
        <$> ("[grey olive, black ~ brown, darkgrey]")
        <*> ( withOpacity
                <$> "[beige, lightblue white darkgreen, beige]"
                <*> ((*) <$> (slow 8 $ slow 4 sinewave1) <*> (slow 3 $ sinewave1))
            )
    )

f =
  density 2 $
    ( flip darken
        <$> (density 8 $ "[black blue, grey ~ navy, cornflowerblue blue]*2")
        <*> sinewave1
    )

g = density 2 $
  do
    let x = "[skyblue olive, grey ~ navy, cornflowerblue green]"
    coloura <- density 8 x
    colourb <- density 4 x
    slide <- slow 2 sinewave1
    return $ blend slide coloura colourb
