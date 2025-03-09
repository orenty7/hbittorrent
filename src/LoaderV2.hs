{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHC2021 #-}

module LoaderV2 (
  startLoader,
  Incoming (..),
  Outgoing (..)
) where

import qualified Torrent as T
import Protocols (PeerMessage (..))
import Storage

import qualified Control.Concurrent.STM as STM
import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Network.Socket as NS

import Control.Lens
import Data.IORef
import Data.List (nub)
import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import System.Random

type PeerId = Int
data Peer = Peer 
  { _id :: PeerId
  , _addr :: NS.SockAddr
  , _rating :: Int
  , _choke :: Bool
  , _interested :: Bool
  , _pieces :: S.Set Word32
  , _queuedPieces :: S.Set Word32
  } deriving Show
makeLenses ''Peer


data Loader = Loader 
  { _storage :: Storage 
  , _torrent :: T.Torrent
  , _peers :: M.Map PeerId Peer
  , _knownPeers :: S.Set NS.SockAddr
  , _loadedPieces :: S.Set Word32
  , _requestedPieces :: S.Set Word32
  , _rng :: StdGen
  } deriving Show
makeLenses ''Loader

data Incoming 
  = InMessage PeerId PeerMessage
  | PeerDied PeerId
  | Heartbeat
  | Connected PeerId NS.SockAddr 
  | DhtPeers [NS.SockAddr]
  | TrackerPeers [NS.SockAddr]
  | FsResponse PeerId Word32 Word32 B.ByteString
  deriving Show

data Outgoing 
  = OutMessage PeerId PeerMessage
  | PieceLoaded Int B.ByteString
  | Connect NS.SockAddr
  | AskDhtPeers
  | AnnounceTracker
  | GetFs PeerId Word32 Word32 Word32
  deriving Show

type L = StateT Loader (Writer [Outgoing])

buildPieceRequests :: T.Torrent -> Word32 -> L [PeerMessage]
buildPieceRequests torrent pieceIdx = do 
  let 
    subpiecesCount = fromIntegral $ subpiecesInPiece torrent pieceIdx
    
    buildRequest subpieceIdx = 
      Request pieceIdx (subpieceIdx * subpieceLength) subpieceLength  

  return $ map buildRequest [0..subpiecesCount - 1]


tryToRequestPiece :: PeerId -> L ()
tryToRequestPiece pid = do
  t <- use torrent
  loaded <- use loadedPieces
  requested <- use requestedPieces
  
  peerPieces <- use $ peers.(ix pid).pieces
  requestedPeerPieces <- use $ peers.(ix pid).queuedPieces
  
  let 
    endgame = length (t^.T.pieces) - S.size loaded < 10
    
    candidates = 
      if endgame 
        then peerPieces `S.difference` (loaded `S.union` requestedPeerPieces)
        else peerPieces `S.difference` (loaded `S.union` requested) 

  when (S.size requestedPeerPieces < 20 && S.size candidates /= 0) $ do
    selectedIdx <- zoom rng $ state $ randomR (0, S.size candidates - 1)
    let pieceIdx = S.elemAt selectedIdx candidates  
    requests <- do 
      t <- use torrent
      buildPieceRequests t pieceIdx
  
    tell $ map (OutMessage pid) requests

    requestedPieces %= (S.insert pieceIdx)
    peers.(ix pid).queuedPieces %= (S.insert pieceIdx)

loader :: Incoming -> L ()
loader inMsg = 
  case inMsg of
    Heartbeat -> do
      p <- use peers
      kp <- use knownPeers
      when (length kp < 10) $ do
        tell [AskDhtPeers, AnnounceTracker]

      when (M.size p < 20) $ do
        let
          candidates = S.difference kp (S.fromList $ map (view addr) $ M.elems p)
          n = 10 - M.size p
        
        when (S.size candidates > 0) $ do
          (indexes :: [Int]) <- zoom rng $ replicateM n $ state $ randomR (0, S.size candidates - 1)
          
          let 
            selected = map (`S.elemAt` candidates) $ nub indexes

          tell $ map Connect selected

    PeerDied pid -> do
      p <- use peers
      peers %= (M.delete pid)
      requestedPieces %= (`S.difference` (p^.(ix pid).queuedPieces))      

    DhtPeers peers -> do
      knownPeers %= (<> S.fromList peers)
    TrackerPeers peers -> do
      knownPeers %= (<> S.fromList peers)
    FsResponse pid index offset bytes -> do
      tell $ [OutMessage pid $ Piece index offset bytes]
    
    Connected pid addr  -> do
      peers %= (M.insert pid $ Peer {
        _id = pid,
        _addr = addr,
        _rating = 0,
        _choke = True,
        _interested = False,
        _pieces = mempty,
        _queuedPieces = mempty 
      })
      
      p <- use (torrent.T.pieces)
      tell $ [
          OutMessage pid $ BitField $ map (const False) $ A.elems p,
          OutMessage pid $ UnChoke,
          OutMessage pid $ Interested
        ]

    InMessage pid msg ->
      case msg of
        Cancel{} ->
          return ()
        KeepAlive ->
          return ()
        Choke -> do
          peers.(ix pid).choke .= True
        UnChoke -> do
          peers.(ix pid).choke .= False
          tryToRequestPiece pid
            
        Interested -> do
          peers.(ix pid).interested .= True
        NotInterested -> do
          peers.(ix pid).interested .= False
        BitField flags -> do
          peers.(ix pid).LoaderV2.pieces .= 
            (S.fromList $ [index | (index, flag) <- zip [0 ..] flags, flag]) 
        Have piece ->
          peers.(ix pid).LoaderV2.pieces %= (S.insert piece)
        Request index offset length -> do
          when (length == subpieceLength) $ do
            tell $ [GetFs pid index offset length]
        
        Piece pieceIdx offset payload -> do
          let 
            offsetIsCorrect = offset `mod` subpieceLength == 0
            sizeIsCorrect = B.length payload == fromIntegral subpieceLength
            subpieceIdx = offset `div` subpieceLength 
            subpiece = Subpiece pieceIdx subpieceIdx payload

          when (offsetIsCorrect && sizeIsCorrect) $ do
            outcome <- zoom storage (state $ store subpiece)

            case outcome of
              Stored -> return ()
              Failed -> do
                requestedPieces %= (S.delete pieceIdx)
                peers.(ix pid).queuedPieces %= (S.delete pieceIdx)                
                
                tryToRequestPiece pid
              
              Finished piece -> do
                tell [PieceLoaded (fromIntegral pieceIdx) piece]
                
                loadedPieces %= (S.insert pieceIdx)
                requestedPieces %= (S.delete pieceIdx)
                peers.(ix pid).queuedPieces %= (S.delete pieceIdx)                
                
                tryToRequestPiece pid
                tryToRequestPiece pid

run :: L a -> Loader -> (Loader, [Outgoing], a)
run l state = (loader, events, a) where
  ((a, loader), events) = runWriter (runStateT l state) 

startLoader :: T.Torrent -> S.Set Word32 -> STM.TChan Incoming -> STM.TChan Outgoing ->  IO ()
startLoader torrent loaded incoming outgoing = do
  let storage = mkStorage torrent
  rng <- getStdGen
  stateRef <- newIORef (Loader {
    _storage = storage,
    _torrent = torrent,
    _peers = mempty,
    _knownPeers =  mempty,
    _loadedPieces = loaded,
    _requestedPieces = mempty,
    _rng = rng
  })

  forever $ do
    inMsg <- STM.atomically $ STM.readTChan incoming
    state <- readIORef stateRef
    
    -- putStrLn $ take 100 $ show inMsg
    let (state', events, ()) = run (loader inMsg) state

    writeIORef stateRef state' 
    STM.atomically $ do
      mapM (STM.writeTChan outgoing) events