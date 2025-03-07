{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GHC2021 #-}


module Storage (
  Storage, 
  Outcome (..), 
  mkStorage,
  store,
  
  Subpiece (..),
  pieceIdx, 
  subpieceIdx
  ) where

import Torrent
import Hash

import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.Map as M

import Control.Lens (set, makeLenses, (^.), view, ix)
import Data.Maybe (fromMaybe)

data Storage = Storage 
  { _mapping :: M.Map Int (M.Map Int B.ByteString)
  , _torrent :: Torrent
  } deriving Show
makeLenses ''Storage

subpieceLength :: Int
subpieceLength = 2^(14 :: Int)

data Subpiece = Subpiece 
  { _pieceIdx :: Int
  , _subpieceIdx :: Int
  , _payload :: B.ByteString } deriving Show
makeLenses ''Subpiece


data Outcome 
  = Finished B.ByteString
  | Failed
  | Stored
  deriving Show

mkStorage :: Torrent -> Storage
mkStorage torrent = Storage mempty torrent

store :: Subpiece -> Storage -> (Outcome, Storage)
store (Subpiece {..}) storage = 
  let 
    piecesMap = storage^.mapping
    subpiecesMap = fromMaybe mempty (M.lookup _pieceIdx piecesMap)
    subpiecesMap' = M.insert _subpieceIdx _payload subpiecesMap 
    piecesMap' = M.insert _pieceIdx subpiecesMap' piecesMap
    storage' = set mapping piecesMap' storage
  in
    (Stored, storage)

  -- subpieceMap = M.findWithDefault mempty index pieceMap

  -- piece = storage^..torrent.pieces.(ix 10) 