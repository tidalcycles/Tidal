{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Edit where

{- Feedforward (c) Alex McLean 2018
   Text editor for TidalCycles
   https://github.com/yaxu/feedforward
   Distributed under the terms of the GNU Public License 3.0, see LICENSE
-}

import Change
import Code
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad (filterM, foldM, forever, join, unless, when)
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.Char
import Data.List
  ( elemIndex,
    inits,
    intercalate,
    isPrefixOf,
    sort,
    stripPrefix,
    (\\),
  )
import Data.Maybe
  ( catMaybes,
    fromJust,
    fromMaybe,
    isJust,
    mapMaybe,
  )
import qualified Data.Text.IO as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Format
import GHC.Generics
import qualified GHC.ST as C
import qualified Network.Socket as N
import qualified Network.WebSockets as WS
import Parameters
import qualified Sound.Osc.Fd as O
import qualified Sound.Osc.Time.Timeout as O
import qualified Sound.Osc.Transport.Fd.Udp as O
import Sound.Tidal.Context hiding (resolve, when)
import System.Directory
import System.Environment (getArgs, lookupEnv)
import System.FilePath
import System.IO
import System.Posix.Process
import System.Posix.Signals
import Text.Printf
import Text.Read (readMaybe)
import TidalHint
import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.Curses as Curses
import UI.HSCurses.CursesHelper (BackgroundColor (DarkBlueB))
import qualified UI.HSCurses.CursesHelper as C

data Dirt = Classic | Super
  deriving (Eq)

data Playback = Playback
  { pbOffset :: Double,
    pbChanges :: [Change]
    -- pbHushTime :: Double
  }
  deriving (Show)

channels = 2

latency = 0.2

dirt = Super

type CpsUtils = (Double -> IO (), Double -> IO (), IO Rational)

data Mode = EditMode | PlaybackMode (Maybe (String, Double))

data FileChoice = FileChoice
  { fcPath :: [FilePath],
    fcIndex :: Int,
    fcDirs :: [FilePath],
    fcFiles :: [FilePath]
  }

data EState = EState
  { sCode :: Code,
    sPos :: Pos,
    sXWarp :: Int,
    sEditWindow :: C.Window,
    sColour :: C.Style,
    sColourBlack :: C.Style,
    sColourHilite :: C.Style,
    sColourWarn :: C.Style,
    sColourShaded :: C.Style,
    sHintIn :: MVar String,
    sHintOut :: MVar Response,
    sTidal :: Stream,
    sChangeSet :: ChangeSet,
    sLogFH :: Handle,
    sRMS :: [Float],
    sScroll :: (Int, Int),
    sMode :: Mode,
    sFileChoice :: FileChoice,
    sCircle :: Maybe (Change -> IO ()),
    sPlayback :: Maybe Playback,
    sName :: Maybe String,
    sNumber :: Maybe Int,
    sRefresh :: Bool,
    sLastAlt :: Double
  }

topMargin :: Int
topMargin = 1

bottomMargin :: Int
bottomMargin = 2

leftMargin :: Int
leftMargin = 3

rightMargin :: Int
rightMargin = 0

feedforward :: Parameters -> IO ()
feedforward params = do
  installHandler sigINT Ignore Nothing
  installHandler sigTERM Ignore Nothing
  installHandler sigPIPE Ignore Nothing
  installHandler sigHUP Ignore Nothing
  installHandler sigKILL Ignore Nothing
  installHandler sigSTOP Ignore Nothing
  installHandler sigTSTP Ignore Nothing
  do
    mvS <- initEState params
    liftIO $ forkIO $ listenRMS mvS
    drawEditor mvS
    -- render
    C.refresh
    mainLoop mvS

sendTidal :: EState -> ControlPattern -> IO ()
sendTidal s pat = do
  let pat' =
        if isJust (sNumber s)
          then pat |+ n (pure $ fromIntegral $ fromJust (sNumber s))
          else pat
  E.catch (streamReplace (sTidal s) "ffwd" pat') (\(e :: E.SomeException) -> hPutStrLn stderr "Hopefully everything is OK")

applyChange :: EState -> Change -> IO (EState)
applyChange s (change@(Change {})) = do
  writeLog s' change
  return s'
  where
    ls
      | (cOrigin change) == Input = updateTags $ applyInput s change
      | (cOrigin change) == Delete = updateTags $ applyDelete s change
      | otherwise = sCode s
    changes = sChangeSet s
    s' =
      s
        { sChangeSet = change : changes,
          sCode = ls,
          sPos = cNewPos change
        }
applyChange s change@(Eval {}) =
  do
    let blocks = unmutedBlocks $ sCode s
        blocks' = allBlocks 0 $ sCode s
    -- hPutStrLn stderr $ "unmuted blocks: " ++ show (length blocks) ++ " of " ++ show (length blocks')
    -- hPutStrLn stderr $ show blocks'
    (s', ps) <- foldM evalBlock (s, []) blocks
    (sendTidal s) (stack ps)
    writeLog s' change
    return s'
applyChange s change@(Snapshot {}) =
  do
    hPutStrLn stderr $ "got a snapshot"
    writeLog s change
    return $
      s
        { sCode = updateTags $ map (Line Nothing) (cText change),
          sPos = (0, 0),
          sRefresh = True
        }
applyChange s change@(Move {}) =
  do
    writeLog s change
    return $ s {sPos = cNewPos change, sXWarp = cXWarp change}
applyChange s change@(MuteToggle {}) =
  do
    writeLog s change
    let ls = sCode s
        ls' = withTag ls (cOrbit change) f
        s' = s {sCode = ls'}
    return s'
  where
    f (l@(Line {lBlock = Just b})) = l {lBlock = Just $ b {bMute = not (bMute b)}}
    f l = l -- can't happen
applyChange s change@(Hush {}) =
  do
    writeLog s change
    (sendTidal s) silence
    return s
applyChange s _ =
  do
    hPutStrLn stderr $ "unhandled change type"
    return s

applyInput :: EState -> Change -> Code
applyInput s change = preL ++ addedWithBlock ++ postL
  where
    (ls, (y, x), preL, l, postL, preX, postX) = cursorContext' s (cFrom change)
    added :: Code
    added = addToHead preX $ addToLast postX $ map (Line Nothing) (cText change)
    addedWithBlock = ((head added) {lBlock = lBlock l}) : (tail added)
    addToHead :: String -> Code -> Code
    addToHead x xs = (withLineText (head xs) (x ++)) : tail xs
    addToLast :: String -> Code -> Code
    addToLast x xs = init xs ++ [withLineText (last xs) (++ x)]

applyDelete :: EState -> Change -> Code
applyDelete s change = preL ++ ((Line (lBlock l) $ preX ++ postX) : postL)
  where
    (_, _, preL, l, _, preX, _) = cursorContext' s (cFrom change)
    (_, _, _, _, postL, _, postX) = cursorContext' s (cTo change)

goCursor :: EState -> IO ()
goCursor state = C.move ((topMargin + fromIntegral (fst $ sPos state)) - sY) ((leftMargin + fromIntegral (snd $ sPos state)) - sX)
  where
    sY = fromIntegral $ fst $ sScroll state
    sX = fromIntegral $ snd $ sScroll state

doScroll :: EState -> (Int, Int) -> EState
doScroll s (h, w) = s {sScroll = (sy', sx')}
  where
    (y, x) = sPos s
    (sy, sx) = sScroll s
    h' = h - (topMargin + bottomMargin)
    w' = w - (leftMargin + rightMargin)
    sy'
      | y < sy = y
      | y >= sy + fromIntegral h' = (y - fromIntegral h') + 1
      | otherwise = sy
    sx'
      | x < sx = x
      | x >= sx + fromIntegral w' = (x - fromIntegral w') + 1
      | otherwise = sx

drawFooter :: EState -> IO ()
drawFooter s =
  do
    -- mc <- maxColor
    let name = fromMaybe "" ((\x -> "[" ++ x ++ "] ") <$> sName s)
        win = sEditWindow s
    -- (h, w) <- windowSize
    (h, w) <- C.scrSize
    C.wMove win (h - 2) 0
    -- setColor $ sColourHilite s
    let str = " " ++ name ++ show (sPos s)
    C.wAddStr win $ str ++ replicate ((fromIntegral w) - (length str)) ' '

rmsBlocks = " ▁▂▃▄▅▆▇█"

-- rmsBlocks = " ░▒▓█"

drawEditor :: MVar EState -> IO ()
drawEditor mvS =
  do
    s <- takeMVar mvS
    -- tempo <- liftIO $ readMVar $ sTempoMV $ sTidal s
    -- t <- liftIO time
    -- let c = timeToCycles tempo (t-latency)
    c <- toRational <$> streamGetNow (sTidal s)
    events <-
      liftIO $
        E.catch
          ( do
              let evs = codeEvents c $ sCode s
              hPrint stderr (length evs)
              return evs
          )
          (\(e :: E.SomeException) -> return [])
    let win = sEditWindow s
    when (sRefresh s) C.refresh
    (h, w) <- C.scrSize
    let s' = doScroll s (h, w)
    -- setColor (sColour s')
    let ls = zip (sCode s) [0 ..]
    mapM_ (drawLine win s w c events) $ zip [topMargin ..] $ take (fromIntegral $ h - (topMargin + bottomMargin)) $ drop (fst $ sScroll s') $ ls
    -- HACK: clear trailing lines in case one (or more) has been deleted
    -- setColor (sColourBlack s')
    when (length ls < (fromIntegral $ h - (bottomMargin + topMargin))) $
      do
        mapM_
          ( \n -> do
              C.move (n + (fromIntegral $ length ls)) 0
              C.wAddStr win $ take (fromIntegral w) $ repeat ' '
          )
          [1 .. fromIntegral $ h - (((fromIntegral $ length ls) + topMargin)) - 1]
    -- drawFooter s''
    goCursor s'
    putMVar mvS (s' {sRefresh = False})
    return ()
  where
    -- drawLine :: EState -> Int -> Rational -> [(Int, Int, Int)] -> (Int, (Line, Int)) -> IO ()
    drawLine win s w c events (y, (l, n)) =
      do
        let scrollX = snd $ sScroll s
            textWidth = w - (leftMargin + rightMargin + 1)
            skipLeft = drop scrollX $ lText l
            skipBoth = take (fromIntegral textWidth) $ skipLeft
        C.move y leftMargin
        -- C.setColor (sColour s)
        C.wAddStr win (take (fromIntegral $ w - leftMargin) skipBoth)
        -- setColor (sColourBlack s)
        C.wAddStr win (replicate (fromIntegral $ w - leftMargin - fromIntegral (length skipBoth)) ' ')
        -- setColor $ sColourHilite s
        let drawEvent (x, x') = when (a >= 0 && a < textWidth) $ do
              C.move y (a + leftMargin)
              C.wAddStr win $ take (fromIntegral $ b - a) $ drop (fromIntegral a) skipBoth
              where
                a = x - (fromIntegral scrollX)
                b = x' - (fromIntegral scrollX)
            lineEvents y = map (\(_, x, x') -> (fromIntegral x, fromIntegral x')) $ filter (\(y', _, _) -> y == (fromIntegral (y' - 1))) events
        mapM_ drawEvent $ lineEvents n
        when (scrollX > 0) $
          do
            C.move y leftMargin
            C.wAddStr win "<"
        when ((length skipLeft) > (length skipBoth)) $
          do
            C.move y (w - 1)
            C.wAddStr win ">"
        C.move y 0
        -- setColor $ sColour s
        lineHead
        drawRMS s w (y - 1) l
      where
        lineHead
          | isJust (lTag l) = do
              let c
                    | lMuted l = return () -- setColor $ sColourShaded s
                    | lStatus l == (Just Error) = return () -- setColor $ sColourWarn s
                    | lStatus l == (Just Success) = return () -- C.setAttribute AttributeBold True
                    | otherwise = return () -- setColor $ sColour s
              C.move y 0
              c
              C.wAddStr win $ (show $ fromJust (lTag l))
              -- setAttribute AttributeBold False
              -- setColor $ sColour s
              C.wAddStr win "│"
          | hasChar l = do
              -- setColor $ sColour s
              C.move y 0
              C.wAddStr win " │"
          | otherwise = do
              C.move y 0
              C.wAddStr win "  "
    drawRMS s w y l
      | hasBlock l = do
          let rmsMax = (length rmsBlocks) - 1
              id = fromJust $ lTag l
              str = map (\n -> rmsBlocks !! (rmsn n)) [0 .. channels - 1]
              rmsn n = min rmsMax $ floor $ 50 * ((sRMS s) !! (id * channels + n))
              win = sEditWindow s
          -- setColor (sColour s)
          C.move (fromIntegral y + topMargin - 1) 0
          C.wAddStr win str
      | otherwise = return ()

connectCircle :: MVar EState -> Maybe String -> IO (Maybe (Change -> IO ()))
connectCircle mvS name =
  do
    addr <- fromMaybe "127.0.0.1" <$> lookupEnv "CIRCLE_ADDR"
    port <- fromMaybe "6010" <$> lookupEnv "CIRCLE_PORT"
    if isJust name
      then do
        mChange <- newEmptyMVar
        forkIO $ WS.runClient addr (read port) "/" (app (fromJust name) mChange)
        return $ Just $ putMVar (mChange :: MVar Change)
      else (return Nothing)
  where
    app name mChange conn =
      do
        -- hPutStrLn stderr "Connected!"
        let msg = T.pack $ "/name " ++ name
        WS.sendTextData conn msg

        forkIO $ forever $ do
          msg <- WS.receiveData conn
          circleAct conn $ T.unpack msg
        -- hPutStrLn stderr $ T.unpack msg
        let loop = do
              change <- takeMVar mChange
              WS.sendTextData conn (T.append (T.pack "/change ") $ decodeUtf8 $ A.encode change) >> loop
        loop
        WS.sendClose conn (T.pack "/quit")
    circleAct conn msg
      | "/takeSnapshot " `isPrefixOf` msg =
          do
            let snapName = fromMaybe "noname" $ stripPrefix "/takeSnapshot " msg
            s <- takeMVar mvS
            let code = map lText $ sCode s
            now <- (realToFrac <$> getPOSIXTime)
            writeLog s $
              Snapshot
                { cWhen = now,
                  cText = code,
                  cName = Just snapName
                }
            putMVar mvS s
            return ()
      | isPrefixOf "/replay " msg =
          do
            let args = words $ fromJust $ stripPrefix "/replay " msg
                session_name = head args
                offset = (read (args !! 1)) :: Double
            liftIO $ do
              -- hPutStrLn stderr $ "args: " ++ show args
              delAll mvS
              s <- takeMVar mvS
              let name = fromMaybe "anon" $ sName s
              -- hPutStrLn stderr $ "replay "
              s' <-
                ( startPlayback s Nothing offset $
                    joinPath
                      [ "sessions",
                        session_name ++ "-" ++ name ++ ".json"
                      ]
                  )
              putMVar mvS s'
            return ()
      | isPrefixOf "/change " msg =
          do
            let change = A.decode $ encodeUtf8 $ T.pack $ fromJust $ stripPrefix "/change " msg
            if (isJust change)
              then do
                s <- takeMVar mvS
                s' <- applyChange s $ fromJust change
                putMVar mvS s'
              else (hPutStrLn stderr $ "bad change: " ++ msg)
            return ()
      | otherwise = return ()

initEState :: Parameters -> IO (MVar EState)
initEState parameters =
  do
    let w = C.stdScr
    C.wclear w
    C.echo False
    C.keypad w True
    let fg = C.Style C.WhiteF C.DarkBlueB
        black = C.Style C.WhiteF C.DefaultB
        bg = C.Style C.BlackF C.WhiteB
        shade = C.Style C.BlackF C.DarkBlueB
        warn = C.Style C.WhiteF C.DarkRedB
    mIn <- liftIO newEmptyMVar
    mOut <- liftIO newEmptyMVar
    liftIO $ forkIO $ hintJob (mIn, mOut) parameters
    tempoIp <- liftIO $ fromMaybe "127.0.0.1" <$> lookupEnv "TEMPO_IP"
    tidal <-
      liftIO $
        startTidal
          (superdirtTarget {oLatency = 0, oAddress = "127.0.0.1", oPort = 57120})
          --          (defaultConfig {cCtrlAddr = "0.0.0.0", cTempoAddr = tempoIp, cFrameTimespan = 1 / 20, cVerbose = False})
          (defaultConfig {cCtrlAddr = "0.0.0.0", cVerbose = False})
    -- liftIO $ streamOnce tidal $ cps 1.05
    -- sock <- liftIO $ carabiner tidal 4 0
    logFH <- liftIO openLog
    name <- liftIO $ lookupEnv "CIRCLE_NAME"
    number <- liftIO $ lookupEnv "CIRCLE_NUMBER"
    mvS <- liftIO $ newEmptyMVar
    circle <- liftIO $ connectCircle mvS name
    liftIO $
      putMVar mvS $
        EState
          { sCode = [Line Nothing ""],
            sPos = (0, 0),
            sEditWindow = w,
            sXWarp = 0,
            sColour = fg,
            sColourBlack = black,
            sColourHilite = bg,
            sColourShaded = shade,
            sColourWarn = warn,
            -- sHilite = (False, []),
            sHintIn = mIn,
            sHintOut = mOut,
            sTidal = tidal,
            sChangeSet = [],
            sLogFH = logFH,
            sRMS = replicate (10 * channels) 0,
            sScroll = (0, 0),
            sMode = EditMode,
            sFileChoice =
              FileChoice
                { fcPath = [],
                  fcIndex = 0,
                  fcDirs = [],
                  fcFiles = []
                },
            sCircle = circle,
            sPlayback = Nothing,
            sName = name,
            sNumber = join $ fmap readMaybe number,
            sRefresh = False,
            sLastAlt = 0
          }
    -- hack to load a json file from a session folder
    when (isJust $ historyFile parameters) $
      do
        let session_file = fromJust $ historyFile parameters
            offset = fromMaybe 0 (historyOffset parameters)
        liftIO $ do
          putStrLn $ "Loading " ++ session_file
          delAll mvS
          s <- takeMVar mvS
          let pbloop = if historyLoop parameters then Just (session_file, offset) else Nothing
          s' <- startPlayback s pbloop offset session_file
          putMVar mvS s'
    return mvS

moveHome :: MVar EState -> IO ()
moveHome mvS = do
  s <- liftIO (readMVar mvS)
  let (_, x) = sPos s
  move mvS (0, negate x)

moveEnd :: MVar EState -> IO ()
moveEnd mvS = do
  s <- liftIO (readMVar mvS)
  let (y, x) = sPos s
      xTo = length (lText $ sCode s !! y)
  move mvS (0, xTo - x)

move :: MVar EState -> (Int, Int) -> IO ()
move mvS (yd, xd) = do
  s <- liftIO (readMVar mvS)
  let maxY = length (sCode s) - 1
      (y, x) = sPos s
      y' = max 0 $ min maxY (y + yd)
      maxX
        | (length $ sCode s) == y' = 0
        | otherwise = length $ lText $ (sCode s) !! y'
      x' = max 0 $ min maxX (x + xd)
      xw
        | xd /= 0 = x'
        | otherwise = sXWarp s
      x'' = min xw maxX
  moveTo mvS (y', x'')

moveTo :: MVar EState -> (Int, Int) -> IO ()
moveTo mvS (y, x) = do
  s <- liftIO (takeMVar mvS)
  let maxY = (length $ sCode s) - 1
      y' = min maxY y
      maxX = length $ lText $ (sCode s) !! y'
      x' = min maxX x
  liftIO $ do
    now <- (realToFrac <$> getPOSIXTime)
    let c =
          Move
            { cWhen = now,
              cNewPos = (y', x'),
              cXWarp = x'
            }
    s' <- applyChange s c
    putMVar mvS s'

openLog :: IO Handle
openLog = do
  t <- getZonedTime
  id <- getProcessID
  let datePath = formatTime defaultTimeLocale ("%Y" </> "%m" </> "%d") t
      time = formatTime defaultTimeLocale "%H%M%S" t
      filePath = logDirectory </> datePath </> time ++ "-" ++ (show id) ++ ".txt"
  createDirectoryIfMissing True (logDirectory </> datePath)
  openFile filePath WriteMode

logDirectory = "logs"

defaultLogPath = do
  t <- getZonedTime
  let y = formatTime defaultTimeLocale "%Y" t
      m = formatTime defaultTimeLocale "%m" t
      d = formatTime defaultTimeLocale "%d" t
      paths = tail $ inits [y, m, d]

  exists <- mapM (doesDirectoryExist . joinPath . (logDirectory :)) paths

  let i = elemIndex False exists
  return $ case i of
    Nothing -> last paths
    Just 0 -> []
    Just n -> paths !! (n - 1)

pathContents path = do
  let fullPath = joinPath (logDirectory : path)
  all <- listDirectory fullPath
  files <- filterM (doesFileExist . (fullPath </>)) all
  dirs <- filterM (doesDirectoryExist . (fullPath </>)) all
  return (dirs, files)

writeLog :: EState -> Change -> IO ()
writeLog s c = do
  hPutStrLn (sLogFH s) (T.unpack $ decodeUtf8 $ A.encode $ c)
  hFlush (sLogFH s)
  sendCircle (sCircle s)
  where
    sendCircle Nothing = return ()
    sendCircle (Just f) = f c

listenRMS :: MVar EState -> IO ()
listenRMS mvS = do
  let port = case dirt of
        Super -> 0
        Classic -> 6010
  udp <- O.udpServer "127.0.0.1" port
  subscribe udp
  loop udp
  where
    loop udp =
      do
        m <- O.recvMessage udp
        act m
        loop udp
    act (Just m@(O.Message "/rmsall" _)) =
      do
        let xs = map (fromMaybe 0 . O.datum_floating) $ O.messageDatum m
        s <- takeMVar mvS
        putMVar mvS $ s {sRMS = xs}
    act (Just m@(O.Message "/rms" _)) =
      do
        s <- takeMVar mvS
        mungeOrbit <- mungeOrbitIO
        let orbit = mungeOrbit $ fromMaybe 0 $ O.datum_integral $ O.messageDatum m !! 1
            chanrms =
              map
                ( \n ->
                    fromMaybe 0 $
                      O.datum_floating $
                        O.messageDatum m !! ((n * 2) + 3)
                )
                [0 .. channels - 1]
            rms = sRMS s
            rms' = take (orbit * channels) rms ++ chanrms ++ drop ((orbit + 1) * (channels)) rms
        putMVar mvS $ s {sRMS = rms'}
    act _ = return ()
    subscribe udp
      | dirt == Super =
          do
            -- remote_addr <- N.inet_addr "127.0.0.1"
            remote_addr <- resolve "127.0.0.1" "57110"
            remote_sockaddr <- N.socket (N.addrFamily remote_addr) (N.addrSocketType remote_addr) (N.addrProtocol remote_addr)
            -- let remote_sockaddr = N.SockAddrInet 57110 remote_addr
            O.sendTo udp (O.p_message "/notify" [O.int32 1]) (N.addrAddress remote_addr)
      | otherwise = return ()

resolve :: [Char] -> [Char] -> IO N.AddrInfo
resolve host port = do
  let hints = N.defaultHints {N.addrSocketType = N.Stream}
  addr : _ <- N.getAddrInfo (Just hints) (Just host) (Just port)
  return addr

-- handleEv :: MVar EState -> Mode -> Maybe UI.NCurses.Event -> Curses Bool
handleEv :: MVar EState -> Mode -> C.Key -> IO Bool
handleEv mvS (PlaybackMode _) ev =
  do
    -- if (isJust ev) then liftIO $ hPutStrLn stderr $ "pressed: " ++ show ev else return ()
    case ev of
      (C.KeyChar x) ->
        if x == '\ESC'
          then do
            s <- liftIO $ takeMVar mvS
            liftIO $ putMVar mvS $ s {sMode = EditMode}
            ok
          else ok
      _ -> ok
handleEv mvS EditMode ev =
  do
    -- if (isJust ev) then liftIO $ hPutStrLn stderr $ "pressed: " ++ show ev else return ()
    case ev of
      C.KeyChar '\ESC' ->
        do
          liftIO $ do
            s <- takeMVar mvS
            now <- (realToFrac <$> getPOSIXTime)
            putMVar mvS $ s {sLastAlt = now}
          ok
      C.KeyChar uArrow -> move mvS (-1, 0) >> ok
      C.KeyChar dArrow -> move mvS (1, 0) >> ok
      C.KeyChar lArrow -> move mvS (0, -1) >> ok
      C.KeyChar rArrow -> move mvS (0, 1) >> ok
      C.KeyHome -> moveHome mvS >> ok
      C.KeyEnd -> moveEnd mvS >> ok
      C.KeyEnter -> insertBreak mvS >> ok
      C.KeyDL -> del mvS >> ok
      C.KeyBackspace -> backspace mvS >> ok
      -- Just (C.KeyChar (KeyFunction 10)) -> unlessKiosk quit
      -- Just (EventMouse _ ms) -> mouse mvS ms >> ok
      C.KeyChar x -> do
        isAlt <- liftIO checkAlt
        keypress mvS isAlt x >> ok
        where
          checkAlt = do
            s <- readMVar mvS
            now <- realToFrac <$> getPOSIXTime
            -- this timeout not actually necessary as
            -- the ESC will do this anyway, via
            -- setKeypad..
            return $ (now - (sLastAlt s)) < altTimeout
          altTimeout = 0.2
      e -> do
        liftIO $ hPrint stderr e
        ok

fcMove mvS d = do
  s <- liftIO $ takeMVar mvS
  let fileChoice = sFileChoice s
      maxI = (length $ fcDirs fileChoice) + (length $ fcFiles fileChoice) - 1
      i = min maxI $ max 0 $ (fcIndex fileChoice) + d
  -- liftIO $ hPutStrLn stderr $ "max: " ++ show maxI ++ " i: " ++ show i
  liftIO $
    putMVar mvS $
      s {sFileChoice = fileChoice {fcIndex = i}}
  return ()

unlessKiosk :: IO Bool -> IO Bool
unlessKiosk f = do
  kiosk <- isJust <$> lookupEnv "KIOSK"
  if kiosk then ok else f

quit :: IO Bool
quit = return True

ok :: IO Bool
ok = return False

mainLoop mvS = loop
  where
    loop = do
      s <- liftIO (readMVar mvS)

      case sMode s of
        EditMode -> drawEditor mvS
        PlaybackMode _ -> drawEditor mvS
      C.refresh

      done <-
        do
          -- ev <- getEvent (sEditWindow s) (Just (1000 `div` 20))
          ev <- C.getCh
          handleEv mvS (sMode s) ev
      updateScreen mvS (sMode s)
      unless done loop

updateScreen :: MVar EState -> Mode -> IO ()
updateScreen mvS (PlaybackMode loopPlayback) =
  do
    s <- liftIO $ takeMVar mvS
    let (Playback offset cs {-hushTime-}) = fromJust $ sPlayback s
    now <- liftIO $ (realToFrac <$> getPOSIXTime)
    -- let cs' = filterPre cs offset
    -- liftIO $ hPutStrLn stderr $ "offset: " ++ show (offset)
    -- liftIO $ hPutStrLn stderr $ "pre: " ++ show (length cs)
    -- liftIO $ hPutStrLn stderr $ "post: " ++ show (length cs')
    -- liftIO $ hPutStrLn stderr $ "cwhen: " ++ (show $ cWhen $ head cs)

    -- liftIO $ hPutStrLn stderr $ "t: " ++ (show $ now - offset)
    let (ready, waiting) = takeReady cs (now - offset)
    s' <- liftIO $ foldM applyChange s ready
    {-liftIO $ if now >= hushTime
             then do hPutStrLn stderr ("hush! " ++ show hushTime)
                     (sendTidal s) silence
                     putMVar mvS (s' {sPlayback = Nothing,
                                      sCode = [],
                                      sXWarp = 0,
                                      sPos = (0,0),
                                      sScroll = (0,0),
                                      sMode = EditMode
                                     }
                                 )
             else -}
    liftIO $ putMVar mvS (s' {sPlayback = Just $ Playback offset waiting {-hushTime-}})
    liftIO $
      when (null waiting && isJust loopPlayback) $
        do
          delAll mvS
          s <- takeMVar mvS
          let (session_file, loopOffset) = fromJust loopPlayback
          s' <- startPlayback s loopPlayback loopOffset session_file
          putMVar mvS s'
    return ()
  where
    takeReady cs t = span (\c -> cWhen c < t) cs
updateScreen _ _ = return ()

-- emacs movement
keyCtrl mvS 'a' = moveHome mvS
keyCtrl mvS 'e' = moveEnd mvS
keyCtrl mvS 'n' = move mvS (1, 0)
keyCtrl mvS 'p' = move mvS (-1, 0)
keyCtrl mvS 'b' = move mvS (0, -1)
keyCtrl mvS 'f' = move mvS (0, 1)
keyCtrl mvS 'd' = del mvS
keyCtrl mvS 'k' = killLine mvS
keyCtrl mvS 'j' = insertBreak mvS
keyCtrl mvS 'x' = eval mvS
-- deprecate?
keyCtrl mvS 'h' = stopAll mvS
keyCtrl mvS 'l' = liftIO $ modifyMVar_ mvS $ \s -> return $ s {sRefresh = True}
keyCtrl mvS '.' = stopAll mvS
keyCtrl mvS _ = return ()

keyAlt mvS '\n' = eval mvS
keyAlt mvS 'h' = stopAll mvS
keyAlt mvS '-' = commentLine mvS
keyAlt mvS '0' = toggleMute mvS 0
keyAlt mvS '1' = toggleMute mvS 1
keyAlt mvS '2' = toggleMute mvS 2
keyAlt mvS '3' = toggleMute mvS 3
keyAlt mvS '4' = toggleMute mvS 4
keyAlt mvS '5' = toggleMute mvS 5
keyAlt mvS '6' = toggleMute mvS 6
keyAlt mvS '7' = toggleMute mvS 7
keyAlt mvS '8' = toggleMute mvS 8
keyAlt mvS '9' = toggleMute mvS 9
keyAlt mvS c = do
  liftIO $ hPutStrLn stderr $ "got Alt-" ++ [c]
  return ()

toggleMute mvS n =
  do
    liftIO $ do
      s <- takeMVar mvS
      now <- liftIO $ (realToFrac <$> getPOSIXTime)
      s' <-
        applyChange s $
          MuteToggle
            { cWhen = now,
              cOrbit = n
            }
      s'' <-
        applyChange s' $
          Eval
            { cWhen = now,
              cAll = True
            }
      putMVar mvS s''

toggleSolo mvS n =
  liftIO $ do
    s <- takeMVar mvS
    let ls = sCode s
        ls' = withTag ls n f
    putMVar mvS (s {sCode = ls'})
  where
    f l@(Line {lBlock = Just b}) = l {lBlock = Just $ b {bSolo = not (bSolo b)}}
    f l = l -- can't happen

{-
keyCtrl mvS c = do s <- (liftIO $ readMVar mvS)
                   updateWindow (sEditWindow s) $ do
                     moveCursor 18 10
                     drawString $ show c
-}

-- mouse mvS (MouseState {mouseCoordinates = (x, y, _), mouseButtons = [(1, ButtonClicked)]}) = moveTo mvS (fromIntegral (max (y - topMargin) 0), fromIntegral (max (x - leftMargin) 0))
-- mouse _ _ = return ()

keypress mvS isAlt c
  | isAlt = keyAlt mvS c
  | isCtrl = keyCtrl mvS (chr $ (ord c) + 96)
  | otherwise = insertChar mvS c
  where
    isCtrl = ord (c) >= 1 && ord (c) <= 26

cursorContext :: EState -> (Code, Pos, Code, Line, Code, String, String)
cursorContext s = cursorContext' s (sPos s)

cursorContext' :: EState -> Pos -> (Code, Pos, Code, Line, Code, String, String)
cursorContext' s (y, x) =
  (ls, (y, x), preL, l, postL, preX, postX)
  where
    ls = sCode s
    preL = take y ls
    l = ls !! max 0 y
    postL = drop (y + 1) ls
    preX = take x $ lText l
    postX = drop x $ lText l

eval :: MVar EState -> IO ()
eval mvS =
  do
    liftIO $ do
      s <- takeMVar mvS
      now <- realToFrac <$> getPOSIXTime
      let change = evalChange {cWhen = now}
      s' <- applyChange s change
      putMVar mvS s'
    return ()

stopAll :: MVar EState -> IO ()
stopAll mvS =
  do
    s <- readMVar mvS
    sendTidal s silence
    return ()

insertBreak :: MVar EState -> IO ()
insertBreak mvS =
  do
    s <- liftIO $ takeMVar mvS
    liftIO $ do
      let (y, x) = sPos s
          (y', x') = (y + 1, 0)
      now <- realToFrac <$> getPOSIXTime
      let change = (insertChange (y, x) ["", ""]) {cWhen = now}
      s' <- applyChange (s {sXWarp = 0}) change
      putMVar mvS s'

insertChar :: MVar EState -> Char -> IO ()
insertChar mvS c =
  do
    s <- liftIO $ takeMVar mvS
    liftIO $ do
      let (y, x) = sPos s
          (y', x') = (y, x + 1)
      now <- realToFrac <$> getPOSIXTime
      let change = (insertChange (y, x) [[c]]) {cWhen = now}
      s' <- applyChange (s {sXWarp = x'}) change
      putMVar mvS s'

-- never called?
backspaceChar :: EState -> EState
backspaceChar s =
  s
    { sCode = ls',
      sPos = (y', x'),
      sXWarp = x'
    }
  where
    (ls, (y, x), preL, l, postL, preX, postX) = cursorContext s
    (y', x') = (y, max 0 (x - 1))
    l'
      | x == 0 = Line Nothing postX
      | otherwise = Line Nothing $ (take ((length preX) - 1) preX) ++ postX
    ls' = preL ++ (l' : postL)

backspace :: MVar EState -> IO ()
backspace mvS =
  do
    s <- takeMVar mvS
    now <- realToFrac <$> getPOSIXTime
    let (y, x) = sPos s
        ls = sCode s
        change
          | x > 0 = (Just $ (deleteChange (y, x - 1) (y, x) [[charAt ls (y, x - 1)]]) {cWhen = now})
          | y == 0 = Nothing
          | otherwise =
              Just $
                ( deleteChange
                    ( y - 1,
                      lineLength ls (y - 1)
                    )
                    (y, x)
                    ["", ""]
                )
                  { cWhen = now
                  }
    s' <- liftIO $ maybe (return s) (applyChange s) change
    liftIO $ putMVar mvS s'

del :: MVar EState -> IO ()
del mvS =
  do
    s <- takeMVar mvS
    now <- realToFrac <$> getPOSIXTime
    let (ls, (y, x), _, l, _, _, _) = cursorContext s
        change
          | x < length (lText l) = Just $ (deleteChange (y, x) (y, x + 1) [[charAt ls (y, x)]]) {cWhen = now}
          | y == (length ls - 1) = Nothing
          | otherwise = Just $ (deleteChange (y, x) (y + 1, 0) ["", ""]) {cWhen = now}
    s' <- liftIO $ maybe (return s) (applyChange s) change
    liftIO $ putMVar mvS s'

delAll :: MVar EState -> IO ()
delAll mvS =
  do
    s <- takeMVar mvS
    now <- realToFrac <$> getPOSIXTime
    let ls = sCode s
        lastY = (length ls) - 1
        lastX = (lineLength ls lastY) - 1
        change
          | null ls = Nothing
          | otherwise = Just $ (deleteChange (0, 0) (lastY, lastX + 1) (map lText ls)) {cWhen = now}
    s' <- maybe (return s) (applyChange s) change
    putMVar mvS (s' {sRefresh = True})

killLine :: MVar EState -> IO ()
killLine mvS =
  do
    s <- (liftIO $ takeMVar mvS)
    now <- (liftIO $ realToFrac <$> getPOSIXTime)
    let (ls, (y, x), _, l, _, _, postX) = cursorContext s
        change
          | x < (length $ lText l) = Just $ deleteChange (y, x) (y, (length $ lText l)) [postX]
          | y == ((length ls) - 1) = Nothing
          | otherwise = Just $ deleteChange (y, x) (y + 1, 0) ["", ""]
    s' <- liftIO $ maybe (return s) (applyChange s) change
    liftIO $ putMVar mvS s'

commentLine :: MVar EState -> IO ()
commentLine = liftIO . commentLine'

commentLine' :: MVar EState -> IO ()
commentLine' mvS = do
  s <- takeMVar mvS
  now <- realToFrac <$> getPOSIXTime
  let (ls, (y, x), _, l, _, _, postX) = cursorContext s
      change
        | isPrefixOf "--" $ lText l = Just $ deleteChange (y, 0) (y, 2) [postX]
        | otherwise = Just $ (insertChange (y, 0) ["-- "]) {cWhen = now}
  s' <- maybe (return s) (applyChange s) change
  putMVar mvS s'

fileTime :: FilePath -> String
fileTime fp = h ++ (':' : m) ++ (':' : s)
  where
    t = take 6 fp
    h = take 2 t
    m = take 2 $ drop 2 t
    s = take 2 $ drop 4 t

evalBlock :: (EState, [ControlPattern]) -> (Int, Code) -> IO (EState, [ControlPattern])
evalBlock (s, ps) (n, ls) = do
  let code = intercalate "\n" (map lText ls)
      id = fromJust $ lTag $ head ls
  liftIO $ putMVar (sHintIn s) code
  response <- liftIO $ takeMVar (sHintOut s)
  -- liftIO $ hPutStrLn stderr $ "Response: " ++ show response
  mungeOrbit <- mungeOrbitIO
  liftIO $ hPutStrLn stderr $ "Id: " ++ show id
  let block = fromJust $ lBlock $ (sCode s) !! n
      (block', ps') = act id (mungeOrbit id) response block
      s' = setBlock n block'
  -- hPutStrLn stderr $ show $ block
  -- hPutStrLn stderr $ ">>>>"
  -- hPutStrLn stderr $ show $ block'
  -- hPutStrLn stderr $ show $ sCode s'
  return (s', ps')
  where
    act id o (HintOK p) b = (b {bStatus = Success, bModified = False, bPattern = Just p'}, p' : ps)
      where
        p' = p # orbit (pure o)
    -- where p' = filt id $ p # orbit (pure o)
    act _ _ (HintError err) b = (b {bStatus = Error}, ps')
      where
        ps'
          | isJust $ bPattern b = (fromJust $ bPattern b) : ps
          | otherwise = ps
    filt i = selectF (cF 0 (show $ 50 + i)) [id, (# (delayfb (cF 0 (show $ 30 + i)) # delayt (select (cF 0 (show $ 70 + i)) [1 / 3, 1 / 6]) # lock 1 # delay (cF 0 (show $ 20 + i))))] . selectF (cF 0 (show $ 90 + i)) [id, (# (room (cF 0 (show $ 40 + i)) # sz 0.8))] . selectF (cF 0 (show $ 80 + i)) [id, (# djf (cF 0 (show $ 40 + i)))]
    {-
    filt id = (|* (lpf (rangex 100 10000 $ min 1 <$> 2 * cF 0 ctrl)
                           |* hpf (range 0 4000 $ max 0 <$> ((2 * cF 0 ctrl) - 1))
                          )/
                      )
              where ctrl = show $ 40 + id
    -}
    setBlock n block = s {sCode = ls'}
      where
        ls = sCode s
        l = (ls !! n) {lBlock = Just block}
        ls' = take n ls ++ (l : (drop (n + 1) ls))

mungeOrbitIO :: IO (Int -> Int)
mungeOrbitIO = do
  orbitOffset <- (read . fromMaybe "0") <$> lookupEnv "ORBIT_OFFSET"
  orbitMax <- (read . fromMaybe "10") <$> lookupEnv "ORBIT_MAX"
  return $ \o -> orbitOffset + (o `mod` orbitMax)

-- -- TODO - it seems this isn't actually called but is duplicated above..
-- scSub = do
--   udp <- O.udpServer "127.0.0.1" 0
--   remote_addr <- resolve "127.0.0.1" "57110"
--   remote_sockaddr <- N.socket (N.addrFamily remote_addr) (N.addrSocketType remote_addr) (N.addrProtocol remote_addr)
--   -- remote_addr <- N.inet_addr "127.0.0.1"
--   -- let remote_sockaddr = N.SockAddrInet 57110 remote_addr
--   O.sendTo udp (O.p_message "/notify" []) (N.addrAddress remote_addr)
--   loop udp
--   where
--     loop udp = do
--       m <- O.recvMessage udp
--       -- hPutStrLn stderr $ show m
--       loop udp

selectedPath fc = fcPath fc ++ [selected]
  where
    selected = (fcDirs fc ++ fcFiles fc) !! fcIndex fc

startPlayback :: EState -> Maybe (String, Double) -> Double -> FilePath -> IO EState
startPlayback s loopPlayback offset path =
  do
    now <- realToFrac <$> getPOSIXTime
    hPutStrLn stderr $ "startplayback: " ++ show loopPlayback
    fh <- openFile (path) ReadMode
    c <- hGetContents fh
    let ls = lines c
        -- ffwdTo = (read (fromJust $ stripPrefix "// " (ls !! 0))) :: Double
        -- hushDelta = (read (fromJust $ stripPrefix "// " (ls !! 1))) :: Double
        changes = mapMaybe (A.decode . encodeUtf8 . T.pack) ls
        ffwdTo = cWhen $ head changes
        -- changes' = filterPre ffwdTo (ffwdTo + hushDelta) changes
        -- offset' = (now - ffwdTo) + offset
        -- hushTime = now + hushDelta
        playback =
          Playback
            { pbChanges = changes,
              pbOffset = (now - ffwdTo) + offset
              {- pbHushTime = hushTime -}
            }
    hPutStrLn stderr $ "offset: " ++ show offset
    -- hPutStrLn stderr $ "ffwdTo: " ++ show ffwdTo
    -- hPutStrLn stderr $ "hushTime: " ++ show hushTime ++ " (" ++ (show (hushTime - now)) ++ ")"
    hPutStrLn stderr $ "now: " ++ show now
    hPutStrLn stderr $ "changes: " ++ show (length changes)
    -- hPutStrLn stderr $ "changes': " ++ show (length changes')
    return $
      s
        { sPlayback = Just playback,
          sMode = PlaybackMode loopPlayback,
          sRefresh = True
        }
  where
    filterPre :: Double -> Double -> [Change] -> [Change]
    filterPre _ _ [] = []
    filterPre start end (c@(Change {}) : cs) = c : (filterPre start end cs)
    filterPre start end (c@(Move {}) : cs) = c : (filterPre start end cs)
    filterPre start end (c : cs)
      | (cWhen c) > end = []
      | (cWhen c) >= start = (c : (filterPre start end cs))
      | otherwise = filterPre start end cs
