module Sound.Tidal.Hint where

import           Control.Exception
import           Language.Haskell.Interpreter as Hint
import           Language.Haskell.Interpreter.Unsafe as Hint
import           Sound.Tidal.Context
import           System.IO
import           Control.Concurrent.MVar
import           Data.List (intercalate,isPrefixOf)
import           Sound.Tidal.Utils
import           System.Environment(lookupEnv)

data Response = HintOK {parsed :: ControlPattern}
              | HintError {errorMessage :: String}

instance Show Response where
  show (HintOK p)    = "Ok: " ++ show p
  show (HintError s) = "Error: " ++ s

runJob :: String -> IO (Response)
runJob job = do putStrLn $ "Parsing: " ++ job
                result <- hintControlPattern job
                let response = case result of
                      Left err -> HintError (show err)
                      Right p -> HintOK p
                return response

libs = [
    "Sound.Tidal.Context"
  , "Sound.Tidal.Simple"
  , "Control.Applicative"
  , "Data.Bifunctor"
  , "Data.Bits"
  , "Data.Bool"
  , "Data.Char"
  , "Data.Either"
  , "Data.Foldable"
  , "Data.Function"
  , "Data.Functor"
  , "Data.Int"
  , "Data.List"
  , "Data.Map"
  , "Data.Maybe"
  , "Data.Monoid"
  , "Data.Ord"
  , "Data.Ratio"
  , "Data.Semigroup"
  , "Data.String"
  , "Data.Traversable"
  , "Data.Tuple"
  , "Data.Typeable"
  , "GHC.Float"
  , "GHC.Real"
  ]

exts = [OverloadedStrings, NoImplicitPrelude]

ghcArgs:: [String]
ghcArgs = ["-clear-package-db", "-package-db", "haskell-libs/package.conf.d", "-package-db", "haskell-libs/package.db", "-v"]

hintControlPattern  :: String -> IO (Either InterpreterError ControlPattern)
hintControlPattern s = do
  env <- lookupEnv "WITH_GHC"
  case env of
    Just "FALSE" -> do
        Hint.unsafeRunInterpreterWithArgsLibdir ghcArgs "haskell-libs" $ do
              Hint.set [languageExtensions := exts]
              Hint.setImports libs
              Hint.interpret s (Hint.as :: ControlPattern)
    _ -> do
        Hint.runInterpreter $ do
          Hint.set [languageExtensions := exts]
          Hint.setImports libs
          Hint.interpret s (Hint.as :: ControlPattern)

hintLoop :: MonadInterpreter m => MVar String -> MVar Response -> m b
hintLoop mIn mOut = do s <- liftIO (readMVar mIn)
                       let munged = deltaMini s
                       t <- Hint.typeChecksWithDetails munged
                       interp t munged
                       hintLoop mIn mOut
             where interp (Left errors) _ = do liftIO $ do putMVar mOut $ HintError $ "Didn't typecheck " ++ concatMap show errors
                                                           hPutStrLn stderr $ "error: " ++ concatMap show errors
                                                           takeMVar mIn
                                                           return ()
                   interp (Right t) s = do p <- Hint.interpret s (Hint.as :: ControlPattern)
                                           liftIO $ putMVar mOut $ HintOK p
                                           liftIO $ takeMVar mIn
                                           return ()

hintJobUnsafe :: MVar String -> MVar Response -> IO ()
hintJobUnsafe mIn mOut =
  do result <- catch (do Hint.unsafeRunInterpreterWithArgsLibdir ghcArgs "haskell-libs" $ do
                           Hint.set [languageExtensions := exts]
                           Hint.setImports libs
                           hintLoop mIn mOut
                     )
               (\e -> return (Left $ UnknownError $ "exception" ++ show (e :: SomeException)))
     let response = case result of
          Left err -> HintError (parseError err)
          Right p  -> HintOK p -- can happen
         parseError (UnknownError s) = "Unknown error: " ++ s
         parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
         parseError (NotAllowed s) = "NotAllowed error: " ++ s
         parseError (GhcException s) = "GHC Exception: " ++ s

     takeMVar mIn
     putMVar mOut response
     hintJobUnsafe mIn mOut



hintJobSafe  :: MVar String -> MVar Response -> IO ()
hintJobSafe mIn mOut =
  do result <- catch (do Hint.runInterpreter $ do
                           Hint.set [languageExtensions := exts]
                           Hint.setImports libs
                           hintLoop mIn mOut
                     )
               (\e -> return (Left $ UnknownError $ "exception" ++ show (e :: SomeException)))
     let response = case result of
          Left err -> HintError (parseError err)
          Right p  -> HintOK p -- can happen
         parseError (UnknownError s) = "Unknown error: " ++ s
         parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
         parseError (NotAllowed s) = "NotAllowed error: " ++ s
         parseError (GhcException s) = "GHC Exception: " ++ s

     takeMVar mIn
     putMVar mOut response
     hintJobSafe mIn mOut

hintJob :: MVar String -> MVar Response -> IO ()
hintJob mIn mOut = do
        env <- lookupEnv "WITH_GHC"
        case env of
          Just "FALSE" -> hintJobUnsafe mIn mOut
          _ -> hintJobSafe mIn mOut
