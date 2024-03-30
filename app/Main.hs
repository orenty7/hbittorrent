{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE BangPatterns #-}

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
import Data.Maybe (isJust)
import System.Environment (getArgs)
import Control.Applicative


readTorrent :: String -> IO (Maybe Torrent)
readTorrent name = do
  bstr <- B.readFile name
  return $ do
    bencode <- parse bstr
    mkTorrent bencode

eval :: [a] -> [a]
eval [] = []
eval list@(x:xs) = (eval xs) `seq` x `seq` list 

main :: IO ()
main = do
  [filename] <- getArgs

  (Just torrent) <- readTorrent filename --"Большой куш Snatch (Гай Ричи Guy Ritchie) [2000, Великобритания, США, криминал, комедия, боевик, WEB-DL 2160p, HDR10, Dolby Vi [rutracker-6375066].torrent"

  lock <- newMVar ()

  let putStrLnPar str = do
        takeMVar lock
        putStrLn str
        putMVar lock ()


  case (view announce torrent) of
    (Just (Left url)) -> do
      announce <- getPeers torrent url 6881

      let connectToPeer (host, port) = connect host (show port)

      let nPieces = (`div` 20) $ B.length $ view Torrent.pieces torrent

      flags <- IO.withFile (T.unpack $ view name torrent) IO.ReadWriteMode $ \handle -> do
        flags <- forM [0 .. nPieces - 1] $ \index -> do
          let hash = torrent^.Torrent.piece index
          let offset = convert index * view pieceLength torrent
          IO.hSeek handle IO.AbsoluteSeek offset
          piece <- B.hGetSome handle (convert $ view pieceLength torrent)

          return $! SHA1.hash piece == hash

        return $ eval flags

      let toLoad = [index | (flag, index) <- (P.zip flags [0 ..]), not flag]
      print toLoad
      
      globalLock <- newTMVarIO ()
      globalEvents <- atomically $ newTChan
      globalState <- newTVarIO $ GlobalState mempty (S.fromList toLoad) globalLock

      waiters <- replicateM 40 newEmptyMVar

      forM_ (P.zip waiters [0 .. 40]) $ \(waiter, i) -> do
        let peer = (view peers announce) !! i
        let action = connectToPeer peer $ \(socket, address) -> do
              putStrLnPar $ show (socket, address)

              stateRef <- socket & (SocketParser.init >=> newIORef)

              void $ performHandshake stateRef socket $ Handshake (Torrent._infoHash torrent) "asdfasdfasdfasdfasdf"

              chan <- atomically $ dupTChan globalEvents
              connectionRef <- newIORef $ Loader.init socket torrent stateRef globalEvents globalState

              send socket $ buildMessage $ Bitfield $ P.replicate ((`div` 20) $ B.length $ view Torrent.pieces torrent) False
              send socket $ buildMessage Unchoke
              send socket $ buildMessage Interested

              forever $ react connectionRef

        void $ forkFinally action $ const $ do
          putMVar waiter ()

      forM_ waiters takeMVar
    _ -> putStrLnPar "Url list is not supported yet"
