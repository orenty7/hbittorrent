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
  subpieceIdx,
  subpieceLength
  ) where

import Torrent
import Hash

import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.Map as M

import Control.Lens (set, makeLenses, (^.))
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
store subpiece storage = 
  let 
    piecesMap = storage^.mapping
    subpiecesMap = fromMaybe mempty (M.lookup (subpiece^.pieceIdx) piecesMap)
    subpiecesMap' = M.insert (subpiece^.subpieceIdx) (subpiece^.payload) subpiecesMap 
    
    nSubpieces = 
      let 
        isLast = length (storage^.torrent.pieces) == (subpiece^.pieceIdx) 
        thisPieceLength = 
          if isLast 
          then (storage^.torrent.fileLength) `mod` (storage^.torrent.pieceLength)
          else (storage^.torrent.pieceLength)
      in
        ceiling (fromIntegral thisPieceLength / fromIntegral subpieceLength)

    (outcome, piecesMap') = 
      case M.size subpiecesMap' == nSubpieces of
        False -> (Stored, M.insert (subpiece^.pieceIdx) subpiecesMap' piecesMap)
        True -> 
          let 
            bytes = mconcat (M.elems subpiecesMap')
            hash = (storage^.torrent.pieces) A.! (subpiece^.pieceIdx)
          in
            case check bytes hash of
              True -> (Finished bytes, M.delete (subpiece^.pieceIdx) piecesMap)
              False -> (Failed , M.delete (subpiece^.pieceIdx) piecesMap)

    storage' = set mapping piecesMap' storage
    
  in
    (outcome, storage')

  -- subpieceMap = M.findWithDefault mempty index pieceMap

  -- piece = storage^..torrent.pieces.(ix 10) 