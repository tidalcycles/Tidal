{-# LANGUAGE TemplateHaskell #-}

module Sound.Tidal.Parse.TH where

import Language.Haskell.TH
import Language.Haskellish
import Control.Monad
import Data.Map

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

-- given the name of a Tidal function, get info about it
reifyTidal :: String -> Q Info
reifyTidal x = reify (mkName $ "T." ++ x)

-- given a list of names of Tidal functions, make a map of that info
-- eg. as a splice that becomes a String: $(reifyTidals ["s","n","jux"] >>= (stringE . show))
reifyTidals :: [String] -> Q (Map String Info)
reifyTidals = sequence . fromList . fmap (\x -> (x,reifyTidal x))
