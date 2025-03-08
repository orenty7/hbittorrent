{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Bencode (parse)
import Dht (find)
import Peer (Handshake (..))
import Torrent
import Utils (convert, timeout, withTcp, createTcp)

import qualified FileSystem as FS
import qualified Loader
import qualified LoaderV2 as V2
import qualified Tracker
import qualified Hash as H

import qualified Control.Concurrent.STM as STM
import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.IORef as IORef
import qualified Data.Set as S
import qualified Data.Map as M

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NS
import qualified Protocols.Core.Message as CoreMessage
import qualified System.IO as IO

import Control.Applicative (asum)
import Control.Concurrent (threadDelay, forkFinally, forkIO, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Lens
import Control.Monad (forM, forM_, forever, join, replicateM, void, when)
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
  let 
    putStrPar str = do
      takeMVar lock
      putStr str
      IO.hFlush IO.stdout
      putMVar lock ()
    
    putStrLnPar str = putStrPar (str <> "\n")
  
  let 
    getAnnouncePeers = do
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
      Dht.find $ H.unHash $ (view Torrent.infoHash torrent)


  filesystem <- FS.mkFileSystem torrent
  
  loaderIn <- STM.newTChanIO
  loaderOut <- STM.newTChanIO
  
  forkIO $ forever $ do
    STM.atomically $ STM.writeTChan loaderIn V2.Heartbeat
    threadDelay 5_000_000

  forkIO $ V2.startLoader torrent loaderIn loaderOut

  counter <- IORef.newIORef 0
  connections <- STM.newTVarIO mempty

  forever $ do
    msg <- STM.atomically $ STM.readTChan loaderOut
    putStrLn $ take 200 $ show msg
    case msg of
      V2.AskDhtPeers -> do
        dhtPeers <- getDhtPeers 
        STM.atomically $ STM.writeTChan loaderIn $ V2.DhtPeers dhtPeers
        print dhtPeers
      
      V2.Connect addr -> do
        pid <- IORef.readIORef counter 
        IORef.writeIORef counter (pid + 1)

        void $ forkIO $ do
          socket <- createTcp
          NS.connect socket addr
          
          let handshake = Handshake (replicate 64 False) (torrent^.infoHash) "asdfasdfasdfasdfasdf"

          Loader.performHandshake socket handshake

          peerIn <- STM.newTChanIO
          peerOut <- STM.newTChanIO

          STM.atomically $ do
            STM.modifyTVar connections (M.insert pid (pid, peerIn, peerOut))

          STM.atomically $ do
            STM.writeTChan loaderIn (V2.Connected pid addr)

      V2.PieceLoaded index piece -> do
        FS.store index piece filesystem

      V2.OutMessage pid msg -> do
        return () 

  let nPieces = length $ view Torrent.pieces torrent

  flags <- IO.withFile (view name torrent) IO.ReadWriteMode $ \handle -> do
    counterRef <- IORef.newIORef (0 :: Integer)

    flags <- forM [0 .. nPieces - 1] $ \index -> do
      let hash = (torrent ^. Torrent.pieces) A.! index
      let offset = convert index * view pieceLength torrent
      IO.hSeek handle IO.AbsoluteSeek offset
      piece <- B.hGetSome handle (convert $ view pieceLength torrent)

      let flag = H.check piece hash
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
    let orEmpty :: Either SomeException [NS.SockAddr] -> [NS.SockAddr]
        orEmpty = fromRight []

    announcePeers <- try (timeout 120_000_000 getAnnouncePeers) <&> orEmpty
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
          NS.connect socket peer

          let handshake = Handshake (replicate 64 False) (torrent^.infoHash) "asdfasdfasdfasdfasdf"

          void $ Loader.performHandshake socket handshake
          
          eventsChan <- STM.atomically $ STM.dupTChan globalEvents
          connectionRef <- IORef.newIORef $ Loader.init socket torrent eventsChan globalState

          void $
            NS.send socket $
              Loader.buildMessage $
                CoreMessage.BitField $
                  map (const False) (A.elems $ view Torrent.pieces torrent)

          void $
            NS.send socket $
              Loader.buildMessage CoreMessage.UnChoke
          void $
            NS.send socket $
              Loader.buildMessage CoreMessage.Interested

          forever $ Loader.react connectionRef

    let unlockWaiter (e :: Either SomeException ()) = do
          putStrLnPar "Peer died"
          putStrLnPar $ show e

          putMVar waiter ()

    void $ forkFinally action unlockWaiter

  forM_ waiters takeMVar
  finished <- STM.atomically $ do
    state <- STM.readTVar globalState
    return $ P.null (view Loader.piecesToLoad state)

  if finished
    then putStrLnPar "Finished"
    else do
      putStrLnPar "Something went wrong, please restart the torrent"
      exitFailure
