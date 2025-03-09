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
  subpieceLength,
  subpiecesInPiece,
  storedPieces
) where

import Torrent
import Hash

import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Lens (set, makeLenses, (^.))
import Data.Maybe (fromMaybe)
import Data.Word

data Storage = Storage 
  { _mapping :: M.Map Word32 (M.Map Word32 B.ByteString)
  , _torrent :: Torrent
  } deriving Show
makeLenses ''Storage

subpieceLength :: Word32
subpieceLength = 2^(14 :: Word32)

data Subpiece = Subpiece 
  { _pieceIdx :: Word32
  , _subpieceIdx :: Word32
  , _payload :: B.ByteString } deriving Show
makeLenses ''Subpiece


data Outcome 
  = Finished B.ByteString
  | Failed
  | Stored
  deriving Show

mkStorage :: Torrent -> Storage
mkStorage torrent = Storage mempty torrent

subpiecesInPiece :: Torrent -> Word32 -> Int
subpiecesInPiece torrent pieceIndex = 
  ceiling @Double (fromIntegral thisPieceLength / fromIntegral subpieceLength) where
    thisPieceLength = nthPieceLength torrent (fromIntegral pieceIndex)

store :: Subpiece -> Storage -> (Outcome, Storage)
store subpiece storage = 
  let 
    piecesMap = storage^.mapping
    subpiecesMap = fromMaybe mempty (M.lookup (subpiece^.pieceIdx) piecesMap)
    subpiecesMap' = M.insert (subpiece^.subpieceIdx) (subpiece^.payload) subpiecesMap 
    
    nSubpieces = subpiecesInPiece (storage^.torrent) (subpiece^.pieceIdx)
      
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

storedPieces :: Storage -> S.Set Word32
storedPieces storage = S.fromList $ M.keys $ storage^.mapping