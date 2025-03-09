{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Bencode (parse)
import Dht (find)
import Peer (Handshake (..), performHandshake, startPeer)
import Torrent
import Utils (createTcp)

import qualified FileSystem as FS
import qualified LoaderV2 as V2
import qualified Tracker
import qualified Hash as H

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as B
import qualified Data.IORef as IORef
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Network.Socket as NS
import qualified System.IO as IO

import Control.Applicative (asum)
import Control.Concurrent (threadDelay, forkIO, newMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Lens
import Control.Monad (forever, join, void)
import System.Environment (getArgs)
import System.Exit (exitFailure)

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
          res <- asum $ P.map (\url -> try $ Tracker.getPeers torrent url 6881) urls
          case (res :: Either SomeException Tracker.Announce) of 
            Right peers -> return peers
            Left _ -> fail "Can't request peers"

      return (view Tracker.peers announce)

    getDhtPeers = do
      putStrLnPar "Connecting to the DHT..."
      Dht.find $ H.unHash $ (view Torrent.infoHash torrent)

  IO.hSetBuffering IO.stdout IO.NoBuffering

  filesystem <- FS.mkFileSystem torrent
  loadedPieces <- FS.check filesystem 

  loaderIn <- STM.newTChanIO
  loaderOut <- STM.newTChanIO
  
  forkIO $ forever $ do
    STM.atomically $ STM.writeTChan loaderIn V2.Heartbeat
    threadDelay 5_000_000

  forkIO $ V2.startLoader torrent loadedPieces loaderIn loaderOut

  counterRef <- IORef.newIORef 0
  connectionsRef <- STM.newTVarIO mempty

  forkIO $ forever $ do
    (pid, msg) <- STM.atomically $ do
      peers <- M.toList <$> STM.readTVar connectionsRef
      
      asum $ flip map peers $ \(pid, (peerIn, peerOut)) -> do
        peerMsg <- STM.readTChan peerOut
        return (pid, peerMsg)
    

    STM.atomically $ STM.writeTChan loaderIn $ 
      case msg of
        Just msg -> V2.InMessage pid msg
        Nothing -> V2.PeerDied pid
    
    case msg of
      Nothing -> putStrLn "peer died"
      _ -> return ()

  finishedRef <- IORef.newIORef $ S.size loadedPieces
 
  forever $ do
    msg <- STM.atomically $ STM.readTChan loaderOut 
    -- putStrLn $ take 100 $ show msg

    case msg of
      V2.AnnounceTracker -> do
        void $ forkIO $ do 
          peers <- getAnnouncePeers 
          STM.atomically $ STM.writeTChan loaderIn $ V2.DhtPeers peers
        
      V2.AskDhtPeers -> do
        void $ forkIO $ do 
          peers <- getDhtPeers 
          STM.atomically $ STM.writeTChan loaderIn $ V2.TrackerPeers peers
      
      V2.Connect addr -> do
        pid <- IORef.readIORef counterRef 
        IORef.writeIORef counterRef (pid + 1)

        void $ forkIO $ do
          socket <- createTcp

          res <- try $ do
            NS.connect socket addr
            let handshake = Handshake (replicate 64 False) (torrent^.infoHash) "asdfasdfasdfasdfasdf"
            performHandshake socket handshake
            startPeer socket

          case res of
            Left (e :: SomeException) -> 
              STM.atomically $ do
                STM.writeTChan loaderIn (V2.ConnectionFailed addr)
            
            Right (peerIn, peerOut) -> do 
              STM.atomically $ do
                STM.modifyTVar connectionsRef (M.insert pid (peerIn, peerOut))

              STM.atomically $ do
                STM.writeTChan loaderIn (V2.Connected pid addr)

      V2.PieceLoaded index piece -> do
        FS.store index piece filesystem
        IORef.modifyIORef finishedRef (+1)

        finished <- IORef.readIORef finishedRef
        putStrLnPar $ "Loading (" <> show finished <> "/" <> show (length $ torrent^.pieces) <> ")"

      V2.GetFs id index offset length -> do
        response <- FS.get index offset length filesystem
        case response of
          Just bytes -> STM.atomically $ STM.writeTChan loaderIn $ V2.FsResponse id index offset bytes
          Nothing -> return ()

      V2.OutMessage pid msg -> do
        connections <- STM.atomically $ STM.readTVar connectionsRef
        
        case M.lookup pid connections of
          Nothing -> return ()
          Just (peerIn, peerOut) -> STM.atomically $ STM.writeTChan peerIn msg 