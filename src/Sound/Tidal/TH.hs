{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Sound.Tidal.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

bp :: QuasiQuoter
bp = QuasiQuoter {
  quoteExp  = compile,
  quotePat  = notHandled "patterns",
  quoteType = notHandled "types",
  quoteDec  = notHandled "declarations"
}
  where notHandled things = error $
          things ++ " are not handled by the bp quasiquoter."

compile :: String -> Q Exp
compile s = [e| parseBP_E s |]
