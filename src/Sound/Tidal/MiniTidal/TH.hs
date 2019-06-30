{-# LANGUAGE TemplateHaskell #-}

module Sound.Tidal.MiniTidal.TH where

import Language.Haskell.TH
import Sound.Tidal.MiniTidal.ParseExp

-- example: $(fromTidal "jux") ...is translated as... T.jux <$ reserved "jux"
fromTidal :: String -> Q Exp
fromTidal x = do
  let y = return (VarE $ mkName $ "T." ++ x)
  let z = appE [|reserved|] $ return (LitE $ StringL x)
  uInfixE y [|(<$)|] z

-- example: $(fromHaskell "+") ...is translated as... + <$ reserved "+"
fromHaskell :: String -> Q Exp
fromHaskell x = do
  let y = return (VarE $ mkName $ x)
  let z = appE [|reserved|] $ return (LitE $ StringL x)
  uInfixE y [|(<$)|] z
