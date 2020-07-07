module Sound.Tidal.Hint where

import           Control.Exception
import           Language.Haskell.Interpreter as Hint
import           Sound.Tidal.Context
import           System.IO
import           System.Posix.Signals
import           Control.Concurrent.MVar
import           Data.List (intercalate,isPrefixOf)
import           Sound.Tidal.Utils

data Response = HintOK {parsed :: ControlPattern}
              | HintError {errorMessage :: String}

instance Show Response where
  show (HintOK p)    = "Ok: " ++ show p
  show (HintError s) = "Error: " ++ s

{-
runJob :: String -> IO (Response)
runJob job = do putStrLn $ "Parsing: " ++ job
                result <- hintControlPattern job
                let response = case result of
                      Left err -> Error (show err)
                      Right p -> OK p
                return response
-}

libs = ["Prelude","Sound.Tidal.Context","Sound.OSC.Datum",
        "Sound.Tidal.Simple"
       ]

{-
hintControlPattern  :: String -> IO (Either InterpreterError ControlPattern)
hintControlPattern s = Hint.runInterpreter $ do
  Hint.set [languageExtensions := [OverloadedStrings]]
  Hint.setImports libs
  Hint.interpret s (Hint.as :: ControlPattern)
-}

hintJob  :: MVar String -> MVar Response -> IO ()
hintJob mIn mOut =
  do {-installHandler sigINT Ignore Nothing
     installHandler sigTERM Ignore Nothing
     installHandler sigPIPE Ignore Nothing
     installHandler sigHUP Ignore Nothing
     installHandler sigKILL Ignore Nothing
     installHandler sigSTOP Ignore Nothing-}
     result <- catch (do Hint.runInterpreter $ do
                           --_ <- liftIO $ installHandler sigINT Ignore Nothing
                           Hint.set [languageExtensions := [OverloadedStrings]]
                           --Hint.setImports libs
                           Hint.setImportsQ $ (Prelude.map (\x -> (x, Nothing)) libs) ++ [("Data.Map", Nothing)]
                           hintLoop
                     )
               (\e -> return (Left $ UnknownError $ "exception" ++ show (e :: SomeException)))
     let response = case result of
          Left err -> HintError (parseError err)
          Right p  -> HintOK p -- can happen
         parseError (UnknownError s) = "Unknown error: " ++ s
         parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
         parseError (NotAllowed s) = "NotAllowed error: " ++ s
         parseError (GhcException s) = "GHC Exception: " ++ s
         --parseError _ = "Strange error"

     takeMVar mIn
     putMVar mOut response
     hintJob mIn mOut
     where hintLoop = do s <- liftIO (readMVar mIn)
                         let munged = deltaMini s
                         t <- Hint.typeChecksWithDetails munged
                         interp t munged
                         hintLoop
           interp (Left errors) _ = do liftIO $ do putMVar mOut $ HintError $ "Didn't typecheck" ++ (concatMap show errors)
                                                   hPutStrLn stderr $ "error: " ++ (concatMap show errors)
                                                   takeMVar mIn
                                       return ()
           interp (Right t) s =
             do p <- Hint.interpret s (Hint.as :: ControlPattern)
                liftIO $ putMVar mOut $ HintOK p
                liftIO $ takeMVar mIn
                return ()
