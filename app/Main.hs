{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Main (main) where

import Bencode
-- import Network.Socket
-- import Network.Socket.ByteString

-- import Loader

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import Data.IORef
import Data.List
import Data.Map as M
import Data.Maybe (isJust)
import Data.Set as S
import qualified Data.Text as T
import Data.Word
import Dht
import Loader
import Loader (convert)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBs
import Peer
import SocketParser
import System.Environment (getArgs)
import qualified System.IO as IO
import System.Random
import Text.URI
import Torrent
import Tracker
import Prelude as P

readTorrent :: String -> IO (Maybe Torrent)
readTorrent name = do
  bstr <- B.readFile name
  return $ do
    bencode <- parse bstr
    mkTorrent bencode

eval :: [a] -> [a]
eval [] = []
eval list@(x : xs) = (eval xs) `seq` x `seq` list

createTcp = Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol

withTcp action = bracket createTcp Socket.close action

main :: IO ()
main = do
  args <- getArgs
  let filename = case args of
        [filename] -> filename
        _ -> "debian-12.4.0-amd64-DVD-1.iso.torrent"

  (Just torrent) <- readTorrent filename -- "Большой куш Snatch (Гай Ричи Guy Ritchie) [2000, Великобритания, США, криминал, комедия, боевик, WEB-DL 2160p, HDR10, Dolby Vi [rutracker-6375066].torrent"
  lock <- newMVar ()

  let putStrPar str = do
        takeMVar lock
        putStr str
        IO.hFlush IO.stdout
        putMVar lock ()

  let putStrLnPar str = putStrPar (str <> "\n")

  -- case (view announce torrent) of
  -- (Just announceInfo) -> do
  let nPieces = (`div` 20) $ B.length $ view Torrent.pieces torrent

  flags <- IO.withFile (T.unpack $ view name torrent) IO.ReadWriteMode $ \handle -> do
    counterRef <- newIORef 0

    flags <- forM [0 .. nPieces - 1] $ \index -> do
      let hash = torrent ^. Torrent.piece index
      let offset = convert index * view pieceLength torrent
      IO.hSeek handle IO.AbsoluteSeek offset
      piece <- B.hGetSome handle (convert $ view pieceLength torrent)

      let flag = SHA1.hash piece == hash

      when flag $ do
        modifyIORef counterRef (+ 1)

      when (index `mod` 50 == 0) $ do
        counter <- readIORef counterRef
        putStrPar $ "\27[2\27[1G" <> "Checking (" <> show counter <> "/" <> show index <> ")"

      return $! flag

    counter <- readIORef counterRef
    putStrPar $ "\27[2\27[1G" <> "Checking (" <> show counter <> "/" <> show nPieces <> ")"
    return $ eval flags

  let (finished, toLoad) = Data.List.partition fst (P.zip flags [0 ..])
  putStrLnPar ""
  putStrLnPar "Connecting to the DHT..."
  peers <- Dht.find (view Torrent.infoHash torrent)
  putStrLnPar $ "peers: " <> show peers
  -- announce <- case announceInfo of
  --   Left url -> Tracker.getPeers torrent url 6881
  --   Right urlss -> do
  --     let flatten urlss = urlss >>= id
  --     let urls = flatten urlss

  --     asum $ P.map (\url -> Tracker.getPeers torrent url 6881) urls

  globalLock <- newTMVarIO ()
  globalEvents <- atomically newTChan
  globalRandomGen <- newStdGen
  globalState <-
    newTVarIO $
      LoaderState
        []
        mempty
        (S.fromList $ P.map snd toLoad)
        (S.fromList $ P.map snd finished)
        globalLock
        globalRandomGen

  waiters <- replicateM (P.length peers - 1) newEmptyMVar

  putStrLnPar "Connecting to peers..."

  forM_ (P.zip waiters peers) $ \(waiter, peer) -> do
    let action = withTcp $ \socket -> do
          -- addr : _ <- Socket.getAddrInfo Nothing (Just host) (Just $ show port)
          Socket.connect socket peer

          stateRef <- socket & (SocketParser.init >=> newIORef)

          let handshake = Handshake (replicate (8 * 8 - 20) False <> [True] <> replicate 19 False) (Torrent._infoHash torrent) "asdfasdfasdfasdfasdf"

          handshake <- performHandshake stateRef socket handshake
          let flags = (view extentionFlags handshake)

          when ((P.take 8 $ P.drop (5 * 8) flags) !! 4) $ do
            putStrLnPar "Supports extention protocol!"

          chan <- atomically $ dupTChan globalEvents
          connectionRef <- newIORef $ Loader.init socket torrent stateRef chan globalState

          SocketBs.send socket $ buildMessage $ Bitfield $ P.replicate ((`div` 20) $ B.length $ view Torrent.pieces torrent) False
          SocketBs.send socket $ buildMessage Unchoke
          SocketBs.send socket $ buildMessage Interested

          forever $ react connectionRef

    let unlockWaiter = putMVar waiter ()

    void $ forkFinally action (const unlockWaiter)

  forM_ waiters takeMVar
  finished <- atomically $ do
    state <- readTVar globalState
    return $ P.null (view piecesToLoad state)

  if finished
    then putStrLnPar "Finished"
    else putStrLnPar "Something went wrong, please restart the torrent"
