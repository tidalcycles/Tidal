{- Prototype for #245

This is an in-browser applicaion: you start this executable
and then point your browser to  http://127.0.0.1:8023/

The application will open a connection to supercollider,
see function main below.

The input text-area contains a Haskell expression  x  of type ParamPattern.
This  x  will be evaluated, and "d1" will be applied to it.

Also,  x  will be parsed, and printed in such a way
that the location of all strings in that expression is known,
and locations can be highlighted.

Then, the standard "d1" function will be replaced by some
customized function that produces highlighting information.

Status as of 2017-Nov-12
* basic architecture, sending data to sc works
* todo: parsing, highlighting

-}

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad (void)
import System.IO
import qualified Language.Haskell.Interpreter as LHI
import qualified Sound.Tidal.Context as STC

main :: IO()
main = do  
  (cps, getNow) <- STC.bpsUtils
  (d1, t1) <- STC.superDirtSetters getNow
  
  startGUI defaultConfig $ setup d1

setup :: (STC.ParamPattern -> IO()) -> Window -> UI ()
setup d1 window = void $ do
  
  return window # set title "tidal highlight"
  
  let initial_input
        = "s \"[bd*2 sn*4?, sine?]\" # room \"0.9 0.0\" # delay 0.75 # delaytime 0.75"
  input <- UI.textarea # set value initial_input
  submit <- UI.button #+ [ string "submit" ]
  output <- UI.div #+ [ string "output goes here" ]  
  errors <- UI.div #+ [ string "errors go here" ]

  on UI.click submit $ \ _ -> do
    contents <- get value input
    res <- liftIO $ do
      hPutStrLn stderr contents
      LHI.runInterpreter $ do
        LHI.setImports [ "Sound.Tidal.Context", "Data.Map" ]
        LHI.set [ LHI.languageExtensions LHI.:= [ LHI.OverloadedStrings ] ]
        LHI.interpret contents (LHI.as :: STC.ParamPattern)
    case res of
      Right pp -> do
        liftIO $ do
          hPutStrLn stderr $ show pp
          d1 pp
        element output # set text contents
        element errors # set text ""
      Left  err -> do
        liftIO $ do
          hPutStrLn stderr $ show err
        element errors # set text ( show err )
  
  getBody window #+
    [ UI.h1 #+ [ string "tidal with live highlighting" ]
    , UI.div #+ [ string "prototype for https://github.com/tidalcycles/Tidal/issues/245" ]
    , UI.h2 #+ [ string "Input" ]
    , UI.div #+ [ column [
                    string "must be of type ParamPattern, will be argument to  d1"
                    , return input, return submit
                    ] ]
    , UI.h2 #+ [ string "Output" ]
    , UI.div #+ [ return  output ]
    , UI.h2 #+ [ string "Errors" ]
    , UI.div #+ [ return  errors ]
    ]
