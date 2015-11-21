module Sound.Tidal.MIDI.Control where

import qualified Sound.Tidal.Stream as S

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Params

type RangeMapFunc = (Int, Int) -> Float -> Int

data ControlChange = CC { param :: S.Param, midi :: Int, range :: (Int, Int), vdefault :: Double, scalef :: RangeMapFunc }
           | NRPN { param :: S.Param, midi :: Int, range :: (Int, Int), vdefault :: Double, scalef :: RangeMapFunc }
           | SysEx { param :: S.Param, midi :: Int, range :: (Int, Int), vdefault :: Double, scalef :: RangeMapFunc }

data ControllerShape = ControllerShape {
  controls :: [ControlChange],
  latency :: Double
  }


toOscShape :: ControllerShape -> S.Shape
toOscShape cs =
  let oscparams = [note_p, dur_p, velocity_p] ++ oscparams'
      oscparams' = [param p | p <- (controls cs)]
  in S.Shape {   S.params = oscparams,
                 S.cpsStamp = False,
                 S.latency = latency cs
             }

passThru :: (Int, Int) -> Float -> Int
passThru (_, _) = floor -- no sanitizing of range…

mapRange :: (Int, Int) -> Float -> Int
mapRange (low, high) = floor . (+ (fromIntegral low)) . (* ratio)
  where ratio = fromIntegral $ high - low

mCC :: S.Param -> Int -> ControlChange
mCC p m = CC {param=p, midi=m, range=(0, 127), vdefault=0, scalef=mapRange }

mNRPN :: S.Param -> Int -> ControlChange
mNRPN p m = NRPN {param=p, midi=m, range=(0, 127), vdefault=0, scalef=mapRange }

mrNRPN :: S.Param -> Int -> (Int, Int) -> Double -> ControlChange
mrNRPN p m r d = NRPN {param=p, midi=m, range=r, vdefault=d, scalef=mapRange }

toParams :: ControllerShape -> [S.Param]
toParams shape = map param (controls shape)

ctrlN :: Num b => ControllerShape -> S.Param -> b
ctrlN shape x = fromIntegral $ midi $ paramN shape x

paramN :: ControllerShape -> S.Param -> ControlChange
paramN shape x
  | x `elem` names = paramX x
  | otherwise = error $ "No such Controller param: " ++ show x
  where names = toParams shape
        paramX x = head paramX'
        paramX' = filter ((== x) . param) p
        p = controls shape
