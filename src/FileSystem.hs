{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GHC2021 #-}

module FileSystem (FileSystem, mkFileSystem, savePiece) where

import Torrent

import qualified Data.ByteString as B
import qualified System.IO as IO

import Control.Lens

data FileSystem = FileSystem
  { _torrent :: Torrent
  } deriving Show
makeLenses ''FileSystem

mkFileSystem :: Torrent -> IO FileSystem
mkFileSystem torrent = return $ FileSystem torrent

savePiece :: Int -> B.ByteString -> FileSystem -> IO ()
savePiece index piece fs = do
  IO.withFile (fs^.torrent.name) IO.ReadWriteMode $ \handle -> do
    let offset = (toInteger index) * (fs^.torrent.pieceLength)
    IO.hSeek handle IO.AbsoluteSeek offset
    B.hPut handle piece