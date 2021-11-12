{-# LANGUAGE TemplateHaskell #-}

module Sound.Tidal.Parse.TH where

import Language.Haskell.TH
import Language.Haskellish

-- example: $(fromTidal "jux") ...is translated as... T.jux <$ reserved "jux"
fromTidal :: String -> Q Exp
fromTidal x = do
  let y = return (VarE $ mkName $ "T." ++ x)
  let z = appE [|reserved|] $ return (LitE $ StringL x)
  uInfixE y [|(<$)|] z

-- example: $(fromTidalList ["major","minor"])
fromTidalList :: [String] -> Q Exp
fromTidalList xs = do
  xs' <- mapM fromTidal xs -- :: Q [Exp]
  choice <- [|(<|>)|]
  let f a b = UInfixE a choice b
  return $ foldl1 f xs'

-- example: $(fromHaskell "+") ...is translated as... + <$ reserved "+"
fromHaskell :: String -> Q Exp
fromHaskell x = do
  let y = return (VarE $ mkName $ x)
  let z = appE [|reserved|] $ return (LitE $ StringL x)
  uInfixE y [|(<$)|] z
