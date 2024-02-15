{-# LANGUAGE OverloadedStrings #-}

module Lib ( startServer ) where
import Control.Concurrent (forkFinally, threadDelay)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.Int (Int64)
import qualified Data.Binary as DB
import FRP.Yampa
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Clock (getTime, Clock (Monotonic), toNanoSecs)

createMessage :: Int64 -> S.ByteString -> S.ByteString
createMessage messageType messageBytes = S.toStrict (
  DB.encode messageType <>
  DB.encode (fromIntegral $ S.length messageBytes :: Int64))
    <> messageBytes

data Outerworld = Outerworld { gamepad :: Maybe (Double, Double, Double, Double), scriptReturns :: [(Int, S.ByteString)] }

initialOuterworld = Outerworld { gamepad = Nothing, scriptReturns = [] }

data Innerworld = Innerworld { script :: [S.ByteString] }
initialInnerworld = Innerworld { script = ["print(123499956)"] }

getMsg :: Socket -> Int -> IO S.ByteString
getMsg s n = do
    x <- recv s n
    if S.length x == n then do
      y <- getMsg s n
      return (x <> y)
    else return x

startServer :: IO ()
startServer = runTCPServer (Just "127.0.0.1") "3170" talk
  where
    talk s = do
      timeRef <- newIORef =<< getTime Monotonic
      putStrLn "Connected"
      reactimate (return initialOuterworld)
          (\can_block -> do
            timeDiff <- liftA2 (-) (getTime Monotonic) (readIORef timeRef)
            writeIORef timeRef =<< getTime Monotonic
            return (fromIntegral (toNanoSecs timeDiff) / 1000000000, Just initialOuterworld))
          (\is_changed inner -> do
            mapM_ (sendAll s . createMessage 1) (script inner)
            threadDelay 1000000
            return False)
          (constant initialInnerworld)
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