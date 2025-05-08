{-# LANGUAGE DeriveGeneric #-}

module Change where

import qualified Data.Aeson   as A
import           GHC.Generics

{- Fires every time the content of the editor is changed. The changeObj
is a {from, to, text, removed, origin} object containing information
about the changes that occurred as second argument. from and to are
the positions (in the pre-change coordinate system) where the change
started and ended (for example, it might be {ch:0, line:18} if the
position is at the beginning of line #19). text is an array of strings
representing the text that replaced the changed range (split by
line). removed is the text that used to be between from and to, which
is overwritten by this change. This event is fired before the end of
an operation, before the DOM updates happen.
-}

type Pos = (Int, Int)
data Origin = Input | Delete deriving (Show, Eq, Generic)

data Change = Change {cFrom    :: Pos,
                      cTo      :: Pos,
                      cText    :: [String],
                      cRemoved :: [String],
                      cOrigin  :: Origin,
                      cWhen    :: Double,
                      cNewPos  :: Pos
                     }
            | Eval {cWhen :: Double,
                    -- eval everything or just the local block
                    cAll :: Bool
                   }
            | Move {cWhen   :: Double,
                    cNewPos :: Pos,
                    cXWarp :: Int
                   }
            | MuteToggle {cWhen   :: Double,
                          cOrbit :: Int
                         }
            | Hush {cWhen   :: Double,
                    cOrbit :: Int
                   }
            | Snapshot {cName :: Maybe String,
                        cWhen :: Double,
                        cText :: [String]
                       }
            deriving (Show, Generic)

instance A.ToJSON Origin
instance A.FromJSON Origin
instance A.ToJSON Change
instance A.FromJSON Change

type ChangeSet = [Change]

evalChange :: Change
evalChange = Eval {cWhen = -1, cAll = True}

insertChange :: Pos -> [String] -> Change
insertChange (y,x) str = Change {cFrom = (y,x),
                                 cTo = (y,x),
                                 cText = str,
                                 cRemoved = [""],
                                 cOrigin = Input,
                                 cWhen = -1,
                                 cNewPos = (y',x')
                                }
  where y' = y + length str - 1
        x' | length str == 1 = x + (length $ head str)
           | otherwise = length $ last str

deleteChange :: Pos -> Pos -> [String] -> Change
deleteChange from to removed = Change {cFrom = from,
                                       cTo = to,
                                       cText = [""],
                                       cRemoved = removed,
                                       cOrigin = Delete,
                                       cWhen = -1,
                                       cNewPos = from
                                      }