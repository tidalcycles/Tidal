{-# LANGUAGE TemplateHaskell #-}

module Sound.Tidal.MiniTidal.TH where

import Language.Haskell.TH
import Sound.Tidal.MiniTidal.Token

opParser :: String -> Q Exp
opParser x = do -- op "x" >> return T.x
  let y = appE [|op|] $ return (LitE $ StringL x)
  let z = appE [|return|] $ return (VarE $ mkName $ "T." ++ x)
  uInfixE y [|(>>)|] z
