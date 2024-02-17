{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

createMessage :: Int64 -> S.ByteString -> S.ByteString
createMessage messageType messageBytes = S.toStrict (
  DB.encode messageType <>
  DB.encode (fromIntegral $ S.length messageBytes :: Int64))
    <> messageBytes

globalJoystickIndex :: Int
globalJoystickIndex = 0

evDeviceAdd :: SDL.EventPayload
evDeviceAdd = SDL.JoyDeviceEvent(SDL.JoyDeviceEventData SDL.JoyDeviceAdded (fromIntegral globalJoystickIndex))

evDeviceRemoved :: SDL.EventPayload
evDeviceRemoved = SDL.JoyDeviceEvent(SDL.JoyDeviceEventData SDL.JoyDeviceRemoved (fromIntegral globalJoystickIndex))

startServer :: IO ()
startServer = do
  shutdownRef <- newIORef False

  _ <- forkIO (runTCPServer (Just "127.0.0.1") "3171" (\s -> do
        msg <- recv s 1024
        when (msg == "shutdown") $ do
          writeIORef shutdownRef True
      ))

  SDL.initializeAll

  joystickRef <- newIORef Nothing

  let reloadJoysticks = do
        joysticks <- SDL.availableJoysticks
        unless (null joysticks) $ do
          j <- SDL.openJoystick ((Vector.!) joysticks globalJoystickIndex)
          writeIORef joystickRef (Just j)

  let talk s = do
        timeRef <- newIORef =<< getTime Monotonic
        putStrLn "Connected"
        reactimate (return initialOuterworld)
            (\can_block -> do
              timeDiff <- liftA2 (-) (getTime Monotonic) (readIORef timeRef)
              writeIORef timeRef =<< getTime Monotonic

              evs <- SDL.pollEvents
              when (any (\x -> SDL.eventPayload x == evDeviceAdd) evs) reloadJoysticks

              return (fromIntegral (toNanoSecs timeDiff) / 1000000000,
                Just (initialOuterworld & sdlEvents .~ evs)))
            (\is_changed inner -> do
              mapM_ (sendAll s . createMessage 1) (inner ^. script)
              mapM_ (sendAll s . createMessage 0) (inner ^. pingMessage)
              threadDelay 16666
              return False)
            yaruzoo
        return ()

  _ <- forkIO (runTCPServer (Just "127.0.0.1") "3170" talk)

  let mainLoop = do
        threadDelay 1000000
        shutdown <- readIORef shutdownRef
        unless shutdown mainLoop

  mainLoop

  SDL.quit

  return ()

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
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
        listen sock 1024
        return sock
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)