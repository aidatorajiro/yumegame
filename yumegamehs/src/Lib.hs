{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib ( startServer ) where
import Control.Concurrent (forkFinally, threadDelay, forkIO)
import qualified Control.Exception as E
import Control.Monad ( forever, void, unless, when )
import qualified Data.ByteString as S
import qualified Data.String as S
import Network.Socket (Socket, HostName, ServiceName, AddrInfo (..), withSocketsDo, defaultHints, AddrInfoFlag (..), SocketType(..), close, getAddrInfo, socket, setSocketOption, SocketOption (..), withFdSocket, setCloseOnExecIfNeeded, bind, listen, accept, gracefulClose)
import Network.Socket.ByteString (recv, sendAll)
import Data.Int (Int64)
import qualified Data.Binary as DB
import FRP.Yampa
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Clock (getTime, Clock (Monotonic), toNanoSecs)
import qualified Data.Vector as Vector
import qualified SDL
import Logic
import Control.Lens
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import SDL (InitFlag(InitJoystick), WindowConfig (..))
import Control.Applicative (liftA2)
import Control.Monad.Extra (whenMaybe)
import qualified System.Info as SI
import Data.Maybe (isJust, fromJust)

createMessage :: Int64 -> S.ByteString -> S.ByteString
createMessage messageType messageBytes = S.toStrict (
  DB.encode messageType <>
  DB.encode (fromIntegral $ S.length messageBytes :: Int64))
    <> messageBytes

globalJoystickIndex :: Int
globalJoystickIndex = 0

evDeviceAdd :: SDL.EventPayload
evDeviceAdd = SDL.JoyDeviceEvent (SDL.JoyDeviceEventData SDL.JoyDeviceAdded (fromIntegral globalJoystickIndex))

evDeviceRemoved :: SDL.EventPayload
evDeviceRemoved = SDL.JoyDeviceEvent (SDL.JoyDeviceEventData SDL.JoyDeviceRemoved (fromIntegral globalJoystickIndex))

startServer :: IO ()
startServer = do
  shutdownRef <- newIORef False

  joystickRef <- newIORef Nothing

  evQueue <- newTQueueIO

  SDL.initializeAll

  _ <- forkIO (runTCPServer (Just "127.0.0.1") "3171" (\s -> do
        msg <- recv s 4096
        when (msg == "shutdown") $ do
          writeIORef shutdownRef True
      ))

  let reloadJoysticks = do
        joysticks <- SDL.availableJoysticks
        unless (null joysticks) $ do
          j <- SDL.openJoystick ((Vector.!) joysticks globalJoystickIndex)
          writeIORef joystickRef (Just j)

  mainwindow <- whenMaybe (SI.os == "mingw32") (SDL.createWindow "yumegame window" (SDL.WindowConfig {windowBorder = True, windowHighDPI = False, windowInputGrabbed = False, windowMode = SDL.Windowed, windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL, windowPosition = SDL.Centered, windowResizable = False, windowInitialSize = SDL.V2 250 60, windowVisible = True}))

  renderer <- whenMaybe (isJust mainwindow) (SDL.createRenderer (fromJust mainwindow) (-1) SDL.defaultRenderer)

  let talk s = do
        timeRef <- newIORef =<< getTime Monotonic
        queue_incoming <- atomically newTQueue

        --putStrLn "Connected"

        _ <- forkIO $ forever $ do
          msg <- recv s 4096
          atomically (writeTQueue queue_incoming msg)

        reactimate (return initialOuterworld)
            (\can_block -> do
              timeDiff <- liftA2 (-) (getTime Monotonic) (readIORef timeRef)
              writeIORef timeRef =<< getTime Monotonic

              evss <- atomically $ flushTQueue evQueue
              let evs = concat evss
              when (any (\x -> SDL.eventPayload x == evDeviceAdd) evs) reloadJoysticks

              incoming_packets <- atomically $ flushTQueue queue_incoming

              return (fromIntegral (toNanoSecs timeDiff) / 1000000000,
                Just (initialOuterworld & sdlEvents .~ evs
                                        & incomingMessage .~ incoming_packets)))
            (\is_changed inner -> do
              let mes = map (createMessage 1) (inner ^. script) <> map (createMessage 0) (inner ^. pingMessage)
              sendAll s (S.concat mes)
              threadDelay 16666
              --mapM_ putStrLn (inner ^. debugPrints)
              return False)
            yaruzoo
        return ()

  _ <- forkIO (runTCPServer (Just "127.0.0.1") "3170" talk)

  let mainLoop = do
        evs <- SDL.pollEvents
        atomically (writeTQueue evQueue evs)
        threadDelay 16666
        when (isJust renderer) $ do
          SDL.rendererDrawColor (fromJust renderer) SDL.$= SDL.V4 255 255 255 255
          SDL.clear (fromJust renderer)
          SDL.present (fromJust renderer)
        when (any (\x -> case SDL.eventPayload x of
          SDL.QuitEvent -> True
          _ -> False) evs) (writeIORef shutdownRef True)
        shutdown <- readIORef shutdownRef
        unless shutdown mainLoop

  mainLoop

  when (isJust renderer) $ do
    SDL.destroyRenderer (fromJust renderer)
    SDL.destroyWindow (fromJust mainwindow)

  SDL.quit

  return ()

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loopfunc
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 4096
        return sock
    loopfunc sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)
