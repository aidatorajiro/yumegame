{-# LANGUAGE OverloadedStrings #-}

module Lib ( startServer ) where
import Control.Concurrent (forkFinally, threadDelay)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.Int (Int64)
import qualified Data.Binary as DB

createMessage :: Int64 -> S.ByteString -> S.ByteString
createMessage messageType messageBytes = S.toStrict (
  DB.encode messageType <>
  DB.encode (fromIntegral $ S.length messageBytes :: Int64))
    <> messageBytes

startServer :: IO ()
startServer = runTCPServer (Just "127.0.0.1") "3170" talk
  where
    talk s = do
      putStrLn "Connected"
      talkLoop s
    talkLoop s = do
      let n = 1024
      let msg = do
            x <- recv s n
            if S.length x == n then do
              y <- msg
              return (x <> y)
            else return x
      -- msg' <- msg
      sendAll s (createMessage 0 "p")
      sendAll s (createMessage 1 "print(12345)")
      threadDelay 1000000
      talkLoop s

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