{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GHC2021 #-}

module FileSystem (FileSystem, mkFileSystem, store, check) where

import Torrent
import qualified Hash as H

import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Data.IORef as IORef
import qualified System.IO as IO

import Data.Word
import Control.Lens
import Control.Monad

data FileSystem = FileSystem
  { _torrent :: Torrent
  } deriving Show
makeLenses ''FileSystem

mkFileSystem :: Torrent -> IO FileSystem
mkFileSystem torrent = return $ FileSystem torrent

store :: Int -> B.ByteString -> FileSystem -> IO ()
store index piece fs = do
  IO.withFile (fs^.torrent.name) IO.ReadWriteMode $ \handle -> do
    let offset = (toInteger index) * (fs^.torrent.pieceLength)
    IO.hSeek handle IO.AbsoluteSeek offset
    B.hPut handle piece

check :: FileSystem -> IO (S.Set Word32)
check fs = do
  let (nPieces :: Word32) = fromIntegral $ length (fs^.torrent.pieces)

  IO.withFile (fs^.torrent.name) IO.ReadWriteMode $ \handle -> do
    counterRef <- IORef.newIORef (0 :: Integer)

    result <- forM [0 .. nPieces - 1] $ \index -> do
      let hash = (fs^.torrent.pieces) A.! index
      let offset = index * (fromInteger $ fs^.torrent.pieceLength)
      IO.hSeek handle IO.AbsoluteSeek (fromIntegral offset)
      piece <- B.hGetSome handle (fromInteger $ fs^.torrent.pieceLength)

      let flag = H.check piece hash
      when flag $ do
        IORef.modifyIORef counterRef (+ 1)

      when (index `mod` 50 == 0) $ do
        counter <- IORef.readIORef counterRef
        putStr $ "\27[2\27[1G" <> "Checking (" <> show counter <> "/" <> show index <> ")"

      return $! (flag, index)
    
    counter <- IORef.readIORef counterRef
    putStrLn $ "\27[2\27[1G" <> "Checking (" <> show counter <> "/" <> show nPieces <> ")"
    return $ S.fromList $ map snd $ filter fst result