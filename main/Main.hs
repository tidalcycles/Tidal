{-# language PatternSignatures, LambdaCase #-}

import qualified Language.Haskell.Interpreter as I
import qualified Sound.Tidal.Safe.Context as C
import Control.Monad (void)
import Control.Exception (throw)
import Control.Monad.IO.Class
import Control.Monad.Catch
-- import qualified Mueval.Resources as MR
import System.Timeout
import System.IO
import Data.Char (isSpace)
import Data.List (isPrefixOf)

main :: IO()
main = do
  
  -- from BootTidal.hs:
  tidal <- C.startTidal
    (C.superdirtTarget
      { C.oLatency = 0.1, C.oAddress = "127.0.0.1"
      , C.oPort = 57120})
    (C.defaultConfig {C.cFrameTimespan = 1/20})
    
  void $ I.runInterpreter
    $ catch (core tidal)
    $ \ (e :: SomeException) -> message stderr $ show e

core tidal = do
    message stdout "safe-tidal-cli starts"
  -- more settings at
  -- https://github.com/tidalcycles/tidali/blob/master/src/Main.hs
    I.set [ I.languageExtensions
              I.:= [ I.OverloadedStrings ]
            , I.installedModulesInScope I.:= False
            ]
    I.setImports
      [ "Prelude"
      , "Sound.Tidal.Safe.Context"
      , "Sound.Tidal.Safe.Boot"
      ]
    -- FIXME: replace lazy IO by some streaming mechanism?
    message stdout "safe-tidal-cli has loaded modules"
    input <- liftIO getContents 
    message stdout "safe-tidal-cli has acquired input"
    mapM_ (work tidal . unlines) $ blocks $ lines input
    message stdout "safe-tidal-cli is done"

second = 10^6 :: Int

-- | will show at most 10 lines, at most 80 chars per line,
-- and run (evaluation and print) for at most 1 second
message :: Handle -> String -> I.InterpreterT IO ()
message h s = do
  let safe = unlines . safe_list 10 ["..."] . map (safe_list 120 "...") . lines
  liftIO $ void $ timeout (1 * second) $ do
    hPutStrLn h (safe s) ; hFlush h


-- | if `length xs <= c`, then `xs`, else `xs <> msg`
safe_list :: Int -> [a] -> [a] -> [a]
safe_list n msg xs =
  let (pre,post) = splitAt n xs
  in  if null post then pre
      else pre <> msg

work :: C.Stream -> String -> I.InterpreterT IO ()
work tidal contents = 
        ( if take 2 contents `elem` [ ":t", ":i", ":d" ]
          then do
            -- https://github.com/haskell-hint/hint/issues/101
            message stderr $ "not implemented " <>  contents
          else 
           I.typeChecksWithDetails contents >>= \ case
             Left errs -> throw $ I.WontCompile errs
             Right s ->
               if s == "Op ()" then do -- execute, print nothing
                 -- TODO: need timeout for evaluation of pattern:
                 x <- I.interpret contents (I.as :: C.Op ())
                 -- have timeout for execution of pattern:
                 liftIO $ void $ timeout (1 * second) $ C.exec tidal x
               else do -- print type and value
                 message stdout $ "type : " <> s
                 if isPrefixOf "IO" s then do
                   message stderr "cannot show value, will not execute action"
                 else do
                   v <- I.eval contents
                   message stdout $ "value: " <> v
        )
      `catches`
      [ Handler $ \ (e :: I.InterpreterError) ->
        message stderr $ unlines $ case e of
          I.UnknownError s -> [ "UnknownError", s ]
          I.WontCompile gs -> "WontCompile" : map I.errMsg gs
          I.NotAllowed s   -> [ "NotAllowed", s ]
          I.GhcException s -> [ "GhcException", s ]
      , Handler $ \ (e :: SomeException) ->
        message stderr $ show e
      ]

-- | What is a "block"? Depends on flok,
-- https://github.com/munshkr/flok/issues/64#issuecomment-614589330
blocks :: [String] -> [[String]]
blocks [] = []
blocks css =
  let blank = all isSpace
      (pre, midpost) = span blank css
      (mid, post) = span (not . blank) midpost
  in  mid : blocks post



