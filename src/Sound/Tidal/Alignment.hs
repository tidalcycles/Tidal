module Sound.Tidal.Alignment where

import           Sound.Tidal.Pattern
import           Sound.Tidal.Types

squeeze :: Pattern p => p a -> p a
squeeze = setStrategy SqueezeIn

squeezeIn :: Pattern p => p a -> p a
squeezeIn = squeeze

squeezeOut :: Pattern p => p a -> p a
squeezeOut = setStrategy SqueezeOut

expand :: Pattern p => p a -> p a
expand = setStrategy Expand

repeat :: Pattern p => p a -> p a
repeat = setStrategy Repeat
