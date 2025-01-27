{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Sound.Tidal.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

{-
    TH.hs - An experiment in using template haskell with the mininotation
    Copyright (C) 2020, Alex McLean and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

bp :: QuasiQuoter
bp =
  QuasiQuoter
    { quoteExp = compile,
      quotePat = notHandled "patterns",
      quoteType = notHandled "types",
      quoteDec = notHandled "declarations"
    }
  where
    notHandled things =
      error $
        things ++ " are not handled by the bp quasiquoter."

compile :: String -> Q Exp
compile s = [e|parseBP_E s|]
