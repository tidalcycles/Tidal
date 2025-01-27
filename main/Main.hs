{-
    Tidal REPL - mimicking ghci

    Copyright (C) 2021 Johannes Waldmann and contributors

    Forked from:
    https://github.com/jwaldmann/safe-tidal-cli/

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
{-# LANGUAGE LambdaCase #-}
{-
    Tidal REPL - mimicking ghci

    Copyright (C) 2021 Johannes Waldmann and contributors

    Forked from:
    https://github.com/jwaldmann/safe-tidal-cli/

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
{-# LANGUAGE PatternSignatures #-}

import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.Catch
  ( Handler (Handler),
    MonadCatch (catch),
    SomeException,
    catches,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
-- import qualified Mueval.Resources as MR

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Language.Haskell.Interpreter as I
import qualified Sound.Tidal.Safe.Context as C
import System.IO (Handle, hFlush, hPutStrLn, stderr, stdout)
import System.Timeout (timeout)

main :: IO ()
main = do
  -- from BootTidal.hs:
  tidal <-
    C.startTidal
      ( C.superdirtTarget
          { C.oLatency = 0.1,
            C.oAddress = "127.0.0.1",
            C.oPort = 57120
          }
      )
      (C.defaultConfig {C.cFrameTimespan = 1 / 20})

  void $
    I.runInterpreter $
      catch (core tidal) $
        \(e :: SomeException) -> message stderr $ show e

core :: C.Stream -> I.InterpreterT IO ()
core tidal = do
  message stdout "[tidal] starting..."
  -- more settings at
  -- https://github.com/tidalcycles/tidali/blob/master/src/Main.hs
  I.set
    [ I.languageExtensions
        I.:= [I.OverloadedStrings],
      I.installedModulesInScope I.:= False
    ]
  I.setImports
    [ "Prelude",
      "Sound.Tidal.Safe.Context",
      "Sound.Tidal.Safe.Boot"
    ]
  -- FIXME: replace lazy IO by some streaming mechanism?
  message stdout "[tidal] modules loaded..."
  input <- liftIO getContents
  message stdout "[tidal] ready"
  mapM_ (work tidal . unlines) $ blocks $ lines input
  message stdout "safe-tidal-cli is done"

second :: Int
second = 10 ^ 6 :: Int

-- | will show at most 10 lines, at most 80 chars per line,
-- and run (evaluation and print) for at most 1 second
message :: Handle -> String -> I.InterpreterT IO ()
message h s = do
  let safe = unlines . safe_list 20 ["..."] . map (safe_list 120 "...") . lines
  liftIO $ void $ timeout (1 * second) $ do
    hPutStrLn h (safe s); hFlush h

-- | if `length xs <= c`, then `xs`, else `xs <> msg`
safe_list :: Int -> [a] -> [a] -> [a]
safe_list n msg xs =
  let (pre, post) = splitAt n xs
   in if null post
        then pre
        else pre <> msg

work :: C.Stream -> String -> I.InterpreterT IO ()
work tidal contents =
  ( if take 2 contents `elem` [":t", ":i", ":d", ":s"]
      then do
        -- https://github.com/haskell-hint/hint/issues/101
        message stderr $ "not implemented " <> contents
      else
        I.typeChecksWithDetails contents >>= \case
          Left errs -> throw $ I.WontCompile errs
          Right s ->
            if s == "Op ()"
              then do
                -- execute, print nothing
                -- TODO: need timeout for evaluation of pattern:
                x <- I.interpret contents (I.as :: C.Op ())
                -- have timeout for execution of pattern:
                liftIO $ void $ timeout (1 * second) $ C.exec tidal x
              else do
                -- print type and value
                message stdout $ "type : " <> s
                if isPrefixOf "IO" s
                  then do
                    message stderr "cannot show value, will not execute action"
                  else do
                    v <- I.eval contents
                    message stdout $ "value: " <> v
  )
    `catches` [ Handler $ \(e :: I.InterpreterError) ->
                  message stderr $ unlines $ case e of
                    I.UnknownError s -> ["UnknownError", s]
                    I.WontCompile gs -> "WontCompile" : map I.errMsg gs
                    I.NotAllowed s -> ["NotAllowed", s]
                    I.GhcException s -> ["GhcException", s],
                Handler $ \(e :: SomeException) ->
                  message stderr $ show e
              ]

-- | Mimicking ghci, where a block is wrapped in `:{` and `:}`, on otherwise empty lines.
blocks :: [String] -> [[String]]
blocks [] = []
blocks (":{" : ls) = b : (blocks ls')
  where
    (b, ls') = block ls
blocks (l : ls) = [l] : (blocks ls)

block :: [String] -> ([String], [String])
block [] = ([], [])
block (":}" : ls) = ([], ls)
block (l : ls) = (l : b, ls')
  where
    (b, ls') = block ls
