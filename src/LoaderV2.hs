{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module LoaderV2 () where

-- import Torrent
-- import Peer
-- import Storage

-- import qualified Control.Concurrent.STM as STM
-- import qualified Data.ByteString as B
-- import qualified Data.Map as M


-- import Control.Lens


-- subpieceLength :: Int
-- subpieceLength = 2^(14 :: Int)


-- data Subpiece = Subpiece 
--   { _pieceIdx :: Int
--   , _insideIdx :: Int } deriving Show
-- makeLenses ''Subpiece

-- type PeerId = Int
-- data Peer = Peer 
--   { _id :: PeerId
--   , _rating :: Int
--   , _choke :: Bool
--   , _interested :: Bool
--   , _queuedSubpieces :: [Int]
--   } deriving Show
-- makeLenses ''Peer



-- type Piece = B.ByteString
-- data Outcome 
--   = Finished Piece
--   | Failed
--   | Stored
--   deriving Show

-- data Loader = Loader 
--   { _storage :: Storage 
--   } deriving Show
-- makeLenses ''Loader

-- data Incoming 
--   = InMessage PeerId 
--   | PeerDied PeerId

-- data Outgoing 
--   = OutMessage PeerId 

-- startLoader :: Torrent -> STM.TChan Incoming -> STM.TChan Outgoing ->  IO ()
-- startLoader torrent incoming outgoing = do
--   return ()