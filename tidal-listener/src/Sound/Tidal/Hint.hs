module Sound.Tidal.Hint where

import Control.Exception  (SomeException)
import Control.Monad.Catch (catch)
import Control.DeepSeq (deepseq)
import Control.Concurrent.MVar  (MVar, putMVar, takeMVar)

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Sound.Tidal.Context (Stream)

import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint

import Data.List (intercalate)
import Data.IORef

import Sound.Tidal.Listener.Config
import Sound.Tidal.Listener.Parse

ghcArgs:: String -> [String]
ghcArgs lib = ["-clear-package-db", "-package-db", lib ++ "haskell-libs/package.conf.d", "-package-db", lib ++ "haskell-libs/package.db", "-v"]

unsafeInterpreter :: Interpreter a -> IO (Either InterpreterError a)
unsafeInterpreter interpreter = do
  execPath <- dropFileName <$> getExecutablePath
  Hint.unsafeRunInterpreterWithArgsLibdir (ghcArgs execPath) (execPath ++ "haskell-libs") interpreter

data InterpreterMessage = MStat String
                        | MType String
                        | MLoad String
                        deriving Show

data InterpreterResponse = RStat (Maybe String)
                         | RType String
                         | RError String
                         deriving Show

startHintJob :: Bool -> Stream -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
startHintJob safe str mMV rMV | safe = hintJob Hint.runInterpreter str mMV rMV
                              | otherwise = hintJob unsafeInterpreter str mMV rMV

hintJob :: (Interpreter () -> IO (Either InterpreterError ())) ->  Stream -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
hintJob interpreter str mMV rMV = do
                result <- catch (interpreter $ (staticInterpreter str) >> (interpreterLoop mMV rMV))
                          (\e -> return (Left e))
                -- can this happen? If it happens all definitions made interactively are lost...
                let response = case result of
                        Left err -> RError (parseError err)
                        Right p  -> RError (show p)
                putMVar rMV response
                hintJob interpreter str mMV rMV

-- this is the basic interpreter that will be only loaded once
staticInterpreter :: Stream -> Interpreter ()
staticInterpreter str = do
                    Hint.set [languageExtensions := exts]
                    Hint.setImportsF libs
                    bind "tidal" str
                    Hint.runStmt bootTidal
                    return ()

-- this is the intrepreter receiving and interpreteing messages and sending the results back
interpreterLoop :: MVar InterpreterMessage -> MVar InterpreterResponse -> Interpreter ()
interpreterLoop mMV rMV = do
                    message <- liftIO $ takeMVar mMV
                    case message of
                      MStat cont -> catch (interpretStatement cont rMV) (\e -> liftIO $ putMVar rMV $ RError $ show (e :: SomeException))
                      MType cont -> catch (interpretType cont rMV) (\e -> liftIO $ putMVar rMV $ RError $ show (e :: SomeException))
                      MLoad path -> catch (interpretFile path rMV) (\e -> liftIO $ putMVar rMV $ RError $ show (e :: SomeException))
                    interpreterLoop mMV rMV


interpretStatement :: String -> MVar InterpreterResponse -> Interpreter ()
interpretStatement cont rMV = do
                        t <- Hint.typeChecksWithDetails cont
                        case t of
                          -- if the expression doesn't type check try to just evaluate it (it could be a definition or binding)
                          Left _ -> catch (Hint.runStmt cont >> (liftIO $ putMVar rMV $ RStat Nothing))
                                         (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
                          Right _ -> do
                            Hint.runStmt ("(tmpMsg, !temp) <- hCapture [stderr] $ " ++ cont)
                            out <- Hint.eval "temp"
                            -- force complete evaluation of 'out', so that any possible error is thrown here
                            msg <- deepseq out (Hint.interpret "tmpMsg" (Hint.as :: String))
                            case msg of
                              "" -> liftIO $ putMVar rMV $ RStat (Just out)
                              _ -> liftIO $ putMVar rMV $ RError msg

interpretType :: String -> MVar InterpreterResponse -> Interpreter ()
interpretType cont rMV = do
                  t <- Hint.typeChecksWithDetails cont
                  case t of
                    Left errors -> liftIO $ putMVar rMV $ RError $ intercalate "\n" $ map errMsg errors
                    Right out -> liftIO $ putMVar rMV $ RType out


interpretFile :: String -> MVar InterpreterResponse -> Interpreter ()
interpretFile path rMV = do
                  cont <- liftIO $ readFile path
                  let bs = blocks cont
                  catch ((sequence $ map Hint.runStmt bs) >> (liftIO $ putMVar rMV $ RStat Nothing) >> return ()) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)



parseError:: InterpreterError -> String
parseError (UnknownError s) = "Unknown error: " ++ s
parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
parseError (NotAllowed s) = "NotAllowed error: " ++ s
parseError (GhcException s) = "GHC Exception: " ++ s

bind :: String -> Stream -> Interpreter ()
bind var value = do
  Hint.runStmt "tmpIORef <- newIORef (undefined :: Stream)"
  tmpIORef <- Hint.interpret "tmpIORef" (Hint.as :: IORef Stream)
  liftIO $ writeIORef tmpIORef value
  Hint.runStmt (var ++ " <- readIORef tmpIORef")

runManyStmt :: [String] -> Interpreter ()
runManyStmt [] = return ()
runManyStmt (x:xs) = do
                runStmt x
                runManyStmt xs
