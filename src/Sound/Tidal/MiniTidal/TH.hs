{-# LANGUAGE TemplateHaskell #-}

module Sound.Tidal.MiniTidal.TH where

import Language.Haskell.TH
import Sound.Tidal.MiniTidal.Token

op :: String -> Q Exp
op x = do -- op "x" >> return T.x
  let y = appE [|opParser|] $ return (LitE $ StringL x)
  let z = appE [|return|] $ return (VarE $ mkName $ "T." ++ x)
  uInfixE y [|(>>)|] z

function :: String -> Q Exp
function x = do -- function "x" >> return T.x
  let y = appE [|functionParser|] $ return (LitE $ StringL x)
  let z = appE [|return|] $ return (VarE $ mkName $ "T." ++ x)
  uInfixE y [|(>>)|] z
