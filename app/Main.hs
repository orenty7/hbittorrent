{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Main (main) where

import Bencode (parse)
import Dht (find)
import Peer (Handshake (..))
import SocketParser
import Torrent
import Utils (convert, timeout, withTcp)

import qualified Loader
import qualified Tracker

import qualified Control.Concurrent.STM as STM
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import qualified Data.IORef as IORef
import qualified Data.Set as S
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBs
import qualified Protocols.Core.Message as CoreMessage
import qualified System.IO as IO

import Control.Applicative (asum)
import Control.Concurrent (forkFinally, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Lens
import Control.Monad (forM, forM_, forever, join, replicateM, void, when, (>=>))
import Data.Either (fromRight)
import Data.List (partition)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (newStdGen)

import Prelude as P

readTorrent :: String -> IO (Maybe Torrent)
readTorrent name = do
  bstr <- B.readFile name
  return $ do
    bencode <- parse bstr
    mkTorrent bencode

main :: IO ()
main = do
  args <- getArgs

  filename <- case args of
    [filename] -> return filename
    _ -> do
      putStrLn "Incorrect arguments."
      putStrLn "expected: hbittorrent <torrent>"
      exitFailure

  (Just torrent) <- readTorrent filename
  lock <- newMVar ()

  let putStrPar str = do
        takeMVar lock
        putStr str
        IO.hFlush IO.stdout
        putMVar lock ()

  let putStrLnPar str = putStrPar (str <> "\n")

  let nPieces = (`div` 20) $ B.length $ view Torrent.pieces torrent

  flags <- IO.withFile (view name torrent) IO.ReadWriteMode $ \handle -> do
    counterRef <- IORef.newIORef (0 :: Integer)

    flags <- forM [0 .. nPieces - 1] $ \index -> do
      let hash = torrent ^. Torrent.piece index
      let offset = convert index * view pieceLength torrent
      IO.hSeek handle IO.AbsoluteSeek offset
      piece <- B.hGetSome handle (convert $ view pieceLength torrent)

      let flag = SHA1.hash piece == hash

      when flag $ do
        IORef.modifyIORef counterRef (+ 1)

      when (index `mod` 50 == 0) $ do
        counter <- IORef.readIORef counterRef
        putStrPar $ "\27[2\27[1G" <> "Checking (" <> show counter <> "/" <> show index <> ")"

      return $! flag

    counter <- IORef.readIORef counterRef
    putStrPar $ "\27[2\27[1G" <> "Checking (" <> show counter <> "/" <> show nPieces <> ")"
    return flags

  let (finished, toLoad) = partition fst (P.zip flags [0 ..])
  putStrLnPar ""

  peers <- do
    let getAnnouncePeers = do
          putStrLnPar "Connecting to the Tracker..."
          announce <- case view announce torrent of
            Nothing -> fail "No announce"
            Just (Left url) -> Tracker.getPeers torrent url 6881
            Just (Right urlss) -> do
              let urls = join urlss
              asum $ P.map (\url -> Tracker.getPeers torrent url 6881) urls
          return (view Tracker.peers announce)

        getDhtPeers = do
          putStrLnPar "Connecting to the DHT..."
          Dht.find (view Torrent.infoHash torrent)

    let orEmpty :: Either SomeException [Socket.SockAddr] -> [Socket.SockAddr]
        orEmpty = fromRight []

    announcePeers <- try (timeout 30_000_000 getAnnouncePeers) <&> orEmpty
    dhtPeers <- try (timeout 120_000_000 getDhtPeers) <&> orEmpty

    return $ announcePeers <> dhtPeers

  putStrLnPar $ "peers: " <> show peers

  globalLock <- STM.newTMVarIO ()
  globalEvents <- STM.atomically STM.newTChan
  globalRandomGen <- newStdGen
  globalState <-
    STM.newTVarIO $
      Loader.LoaderState
        mempty
        mempty
        (S.fromList $ P.map snd toLoad)
        (S.fromList $ P.map snd finished)
        globalLock
        globalRandomGen

  waiters <- replicateM (P.length peers - 1) newEmptyMVar

  putStrLnPar "Connecting to peers..."

  forM_ (P.zip waiters peers) $ \(waiter, peer) -> do
    let action = withTcp $ \socket -> do
          Socket.connect socket peer

          stateRef <- socket & (SocketParser.init >=> IORef.newIORef)

          let handshake = Handshake (replicate 64 False) (Torrent._infoHash torrent) "asdfasdfasdfasdfasdf"

          void $ Loader.performHandshake stateRef socket handshake

          eventsChan <- STM.atomically $ STM.dupTChan globalEvents
          connectionRef <- IORef.newIORef $ Loader.init socket torrent stateRef eventsChan globalState

          void $
            SocketBs.send socket $
              Loader.buildMessage $
                CoreMessage.BitField $
                  P.replicate ((`div` 20) $ B.length $ view Torrent.pieces torrent) False

          void $
            SocketBs.send socket $
              Loader.buildMessage CoreMessage.Unchoke
          void $
            SocketBs.send socket $
              Loader.buildMessage CoreMessage.Interested

          forever $ Loader.react connectionRef

    let unlockWaiter = do
          putMVar waiter ()

    void $ forkFinally action (const unlockWaiter)

  forM_ waiters takeMVar
  finished <- STM.atomically $ do
    state <- STM.readTVar globalState
    return $ P.null (view Loader.piecesToLoad state)

  if finished
    then putStrLnPar "Finished"
    else do
      putStrLnPar "Something went wrong, please restart the torrent"
      exitFailure
