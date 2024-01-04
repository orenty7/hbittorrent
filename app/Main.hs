{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Main (main) where

import Bencode
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString as B
import Data.ByteString.UTF8 (fromString)
import Data.IORef
import Data.Text as T
import Network.HTTP.Client (Request)
import Network.HTTP.Req
import System.IO
import System.Random (randomRIO)
import Torrent
import Prelude as P

test :: IO (Maybe Torrent)
test = do
  bstr <- B.readFile "archlinux-2024.01.01-x86_64.iso.torrent"

  return $ do
    bencode <- parse bstr
    mkTorrent bencode

addRangeHeaders :: (Monad m) => Integer -> Integer -> Request -> m Request
addRangeHeaders start end request = do
  let name = "Range"
  let value = "bytes=" <> fromString (show start) <> "-" <> fromString (show end)

  return $ attachHeader name value request

readPiece :: QSem -> Handle -> Integer -> Integer -> IO ByteString
readPiece lock handle pieceLength index = do
  waitQSem lock
  hSeek handle AbsoluteSeek $ index * pieceLength
  bstr <- B.hGetSome handle (fromInteger pieceLength)
  signalQSem lock

  return bstr

writePiece :: QSem -> Handle -> Integer -> Integer -> ByteString -> IO ()
writePiece lock handle pieceLength index piece = do
  waitQSem lock
  hSeek handle AbsoluteSeek $ index * pieceLength
  B.hPut handle piece
  hFlush handle
  signalQSem lock

main :: IO ()
main = do
  (Just torrent) <- test

  handle <- openFile "archlinux.iso" ReadWriteMode

  logLock <- newQSem 1
  ioLock <- newQSem 1
  loadState <- newMVar (P.replicate (P.length $ torrent ^. pieces) 0)
  finishedPiecesCounter <- newMVar 0

  piecesToLoad <- newChan
  finishedLock <- newEmptyMVar

  let indexedPieces = P.zip [0 ..] (torrent ^. pieces)
  forM_ indexedPieces (writeChan piecesToLoad)

  let putStrLnPar str = do
        waitQSem logLock
        putStrLn str
        signalQSem logLock

  let shift index = toInteger index * torrent ^. pieceLength
  let loadPiece !index !piece = do
        let start = shift index
        let end = flip (-) 1 $ min (shift $ index + 1) (torrent ^. fileLength)

        let loop :: Integer -> IO (Either (Url 'Http, Option scheme0) (Url 'Https, Option scheme1))
            loop n = do
              when (n > 1000) $ error "Can't find good uri"
              index <- randomRIO (0 :: Int, P.length (torrent ^. urlList) - 1)
              let !peerUri = (torrent ^. urlList) !! index
              case useURI peerUri of
                Just url -> return url
                Nothing -> loop (n + 1)

        url <- loop 0

        case url of
          Left _ -> return Nothing
          Right httpsUrl -> runReq defaultHttpConfig $ do
            liftIO $ putStrLnPar $ T.unpack $ "Loading from url: " <> renderUrl (fst httpsUrl)

            response <- reqCb GET (fst httpsUrl) NoReqBody bsResponse (snd httpsUrl) (addRangeHeaders start end)
            let !bytes = (responseBody response :: ByteString)

            let !hash = SHA1.hash bytes

            return $ if hash == piece then Just bytes else Nothing

  let loader = do
        pair@(index :: Int, piece) <- readChan piecesToLoad

        pieceInFile <- readPiece ioLock handle (torrent ^. pieceLength) (toInteger index)

        if SHA1.hash pieceInFile == piece
          then do
            putStrLnPar $ "Piece " <> show index <> " is already loaded"
            modifyMVar_ loadState $ return . set (ix index) 3
            modifyMVar_ finishedPiecesCounter $ return . (+ 1)
          else do
            modifyMVar_ loadState $ return . set (ix index) 1
            loaded <- catch (loadPiece index piece) $ \(e :: SomeException) -> do
              putStrLnPar $ "An Error ocurred while loading piece #" <> show index
              return Nothing

            case loaded of
              Just bytes -> do
                putStrLnPar $ "Piece #" <> show index <> " finished"
                writePiece ioLock handle (torrent ^. pieceLength) (toInteger index) bytes
              Nothing -> do
                modifyMVar_ loadState $ return . set (ix index) 0
                writeChan piecesToLoad pair

        counter <- readMVar finishedPiecesCounter

        when (counter == P.length (torrent ^. pieces)) $ do
          putMVar finishedLock ()

        loader

  loadingThreads <- replicateM 32 $ forkIO loader

  -- savingThread <- forkIO $ do
  --   let loop = do
  --         (!index, !bytes) <- readChan loadedPieces
  --         putStrLnPar $ "piece #" <> show index <> " loaded"
  --         modifyMVar_ loadState $ return . set (ix index) 3

  --         hSeek handle AbsoluteSeek (shift index)
  --         B.hPut handle bytes
  --         hFlush handle

  --         modifyMVar_ finishedPiecesCounter $ return . (+ 1)
  --         counter <- readMVar finishedPiecesCounter
  --         putStrLnPar $ show counter <> "/" <> show (P.length $ torrent ^. pieces)

  --         when (counter == P.length (torrent ^. pieces)) $ do
  --           putMVar finishedLock ()

  --         loop

  --   loop

  -- updateThread <- forkIO $ do
  --   let update = do
  --         threadDelay (10 ^ 6)
  --         status <- readIORef loadState
  --         putStrLnPar $ P.concat $ P.map show status
  --         update
  --   update

  readMVar finishedLock

  forM_ loadingThreads killThread

-- killThread savingThread

-- killThread updateThread
