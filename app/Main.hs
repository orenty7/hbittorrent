{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Main (main) where

import Bencode
-- import Network.Socket
-- import Network.Socket.ByteString

import Control.Concurrent
-- import Loader

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import Data.IORef
import Data.Map as M
import Data.Set as S
import qualified Data.Text as T
import Data.Word
import Loader
import Loader (convert)
import Network.Simple.TCP
import Peer
import SocketParser
import qualified System.IO as IO
import Text.URI
import Torrent
import Tracker
import Prelude as P

test :: IO (Maybe Torrent)
test = do
  bstr <- B.readFile "debian-12.4.0-amd64-DVD-1.iso.torrent"

  return $ do
    bencode <- parse bstr
    mkTorrent bencode

main :: IO ()
main = do
  (Just torrent) <- test

  lock <- newMVar ()

  let putStrLnPar str = do
        takeMVar lock
        putStrLn str
        putMVar lock ()

  announce <- getPeers torrent
  let connectToPeer (host, port) = connect host (show port)

  let hashes = view Torrent.pieces torrent

  flags <- IO.withFile (T.unpack $ view name torrent) IO.ReadMode $ \handle -> do
    forM (P.zip hashes [0 ..]) $ \(hash, index) -> do
      let offset = convert index * view pieceLength torrent
      IO.hSeek handle IO.AbsoluteSeek offset
      piece <- B.hGetSome handle (convert $ view pieceLength torrent)

      return $ SHA1.hash piece == hash

  let toLoad = [index | (flag, index) <- (P.zip flags [0 ..]), not flag]
  print toLoad

  globalLock <- newTMVarIO ()
  globalEvents <- atomically $ newTChan
  globalState <- newTVarIO $ GlobalState mempty (S.fromList toLoad) globalLock

  waiters <- replicateM 20 newEmptyMVar

  forM_ (P.zip waiters [0 .. 40]) $ \(waiter, i) -> do
    let peer = (view peers announce) !! i
    let action = connectToPeer peer $ \(socket, address) -> do
          putStrLnPar $ show (socket, address)

          stateRef <- socket & (SocketParser.init >=> newIORef)

          void $ performHandshake stateRef socket $ Handshake (Torrent._infoHash torrent) "asdfasdfasdfasdfasdf"

          chan <- atomically $ dupTChan globalEvents
          connectionRef <- newIORef $ Loader.init socket torrent stateRef globalEvents globalState

          send socket $ buildMessage $ Bitfield $ P.replicate (P.length $ view Torrent.pieces torrent) False
          send socket $ buildMessage Unchoke
          send socket $ buildMessage Interested

          forever $ react connectionRef

    void $ forkFinally action $ const $ do
      putMVar waiter ()

  forM_ waiters takeMVar

-- do
--   message <- runParser stateRef (messageDecoder torrent)
--   let str = case message of
--         Bitfield _ -> "Bitfield ..."
--         Piece index begin piece -> "Piece " <> show index <> " " <> show begin <> " ..."
--         _ -> show message

--   putStrLnPar $ "peer #" <> show i <> ": " <> str

--   case message of
--     Bitfield flags -> do
--       unless (flags !! 0) $ do
--         fail "Doesn't have 1st piece"
--     Unchoke -> do
--       forM_ [0 .. 15] $ \i -> do
--         putStrLnPar $ "Sending request #" <> show i
--         send socket $ buildMessage $ Request 0 (fromInteger $ i * 2 ^ 14) (2 ^ 14)
--     _ -> return ()

-- print $ P.map renderStr $ announce ^. peers
-- print $ announce ^. interval

-- close sock
