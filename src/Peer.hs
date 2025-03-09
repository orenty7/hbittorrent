{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHC2021 #-}

--
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Peer (
  Handshake (..), 
  PeerState (..),
  
  sendHandshake, 
  receiveHandshake, 
  sendMessage, 
  receiveMessage, 
  runPeer,
  communicate,
  startPeer
) where

import Parser.Core (runParser)
import Protocols (MessageHeader (..), PeerMessage (..), Serializable (..), headerSize)
import Protocols.Handshake

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S

import Control.Concurrent
import Control.Exception (SomeException)
import Control.Lens
import Control.Monad (when, forever)
import Control.Monad.Reader
import Control.Monad.Writer

import Prelude as P


data PeerState = PeerState
  { _socket :: S.Socket
  }
  deriving (Show)

makeLenses ''PeerState

type Peer = ReaderT PeerState IO

recv :: Int -> Peer B.ByteString
recv 0 = return B.empty
recv n = do
  socket <- asks (view socket)
  let loop chunks 0 = return $ B.concat $ reverse $ chunks
      loop chunks n = do
        chunk <- liftIO $ S.recv socket n

        let loaded = B.length chunk

        when (loaded == 0) $ do
          fail "Socket closed"

        loop (chunk : chunks) (n - B.length chunk)

  loop [] n

send :: B.ByteString -> Peer ()
send bstr = do
  socket <- asks (view socket)
  liftIO $ S.send socket bstr
  return ()

sendHandshake :: Handshake -> Peer ()
sendHandshake handshake = do
  let bytes = execWriter $ serialize handshake
  send $ B.toStrict $ B.toLazyByteString bytes
  return ()

receiveHandshake :: Peer Handshake
receiveHandshake = do
  rawHandshake <- recv handshakeSize

  case runParser parse rawHandshake of
    Nothing -> fail "Handshake failed"
    Just handshake -> return handshake

receiveMessage :: Peer PeerMessage
receiveMessage = do
  rawHeader <- recv headerSize

  case runParser parse rawHeader of
    Nothing -> fail "Header parsing failed"
    Just (MessageHeader size) -> do
      payload <- recv $ fromIntegral size

      runParser parse payload

sendMessage :: PeerMessage -> Peer ()
sendMessage message = do
  let runSerializer :: (Serializable a) => a -> B.ByteString
      runSerializer = B.toStrict . B.toLazyByteString . execWriter . serialize

      payload = runSerializer message
      header = runSerializer $ MessageHeader $ fromIntegral $ (B.length payload)

  send header
  send payload
  return ()

runPeer :: Peer a -> PeerState -> IO a
runPeer action config = runReaderT action config


startPeer :: S.Socket -> IO (STM.TChan PeerMessage, STM.TChan (Maybe PeerMessage))
startPeer socket = do
  let peer = PeerState socket

  incoming <- STM.newTChanIO
  outgoing <- STM.newTChanIO

  forkFinally 
    (do
      (finished :: MVar Int) <- newEmptyMVar

      t1 <- 
        forkFinally 
          (forever $ do
            msg <- STM.atomically $ STM.readTChan incoming 
            runPeer (sendMessage msg) peer)
          (\(res :: Either SomeException ()) -> do
            print res 
            putMVar finished 1) 

      t2 <- 
        forkFinally 
          (forever $ do
            msg <- runPeer receiveMessage peer 
            STM.atomically $ STM.writeTChan outgoing (Just msg))
          (\(res :: Either SomeException ()) -> do
            print res 
            putMVar finished 2) 

      x <- takeMVar finished
      case x of
        1 -> killThread t2
        2 -> killThread t1
        _ -> error "Invalid tread finished in Peer")
    (\_ -> do 
      STM.atomically $ STM.writeTChan outgoing Nothing
      S.close socket)

  return (incoming, outgoing)    
    

-- program :: Peer ()
-- program = do
--   env <- ask
--   incoming <- liftIO $ STM.atomically STM.newTTChan

--   let receiver = forever $ do
--         msg <- receiveMessage
--         liftIO $ STM.atomically $ do
--           STM.writeTChan incoming msg

--   liftIO $ forkIO $ runPeer receiver env

--   return ()

communicate :: S.Socket -> IO (STM.TChan PeerMessage, STM.TChan PeerMessage) 
communicate socket = do
  peerMessages <- STM.newTChanIO
  masterMessages <- STM.newTChanIO

  let runPeer action = runReaderT action (PeerState socket)

  forkIO $ runPeer $ forever $ do
    msg <- receiveMessage
    liftIO $ STM.atomically $ STM.writeTChan peerMessages msg
 
  forkIO $ runPeer $ forever $ do
    msg <- liftIO $ STM.atomically $ STM.readTChan masterMessages
    sendMessage msg

  return $ (peerMessages, masterMessages)