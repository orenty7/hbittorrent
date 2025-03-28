{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE PartialTypeSignatures #-}

module LoaderV2 (
  startLoader,
  Incoming (..),
  Outgoing (..)
) where

import qualified Torrent as T
-- import Logger
import Protocols (PeerMessage (..))
import Storage

import qualified Control.Concurrent.STM as STM
import qualified Data.Array as A
import qualified Data.Array.MArray as A
import qualified Data.Array.ST as A
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Network.Socket as NS

import Control.Lens
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Writer
import Data.IORef
import Data.STRef
import Data.List (nub)
import Data.Word
import System.Random

type PeerId = Int
data Peer = Peer 
  { _pid :: PeerId
  , _addr :: NS.SockAddr
  , _rating :: Int
  , _choked :: Bool
  , _interested :: Bool
  , _pieces :: S.Set Word32
  , _queuedSubpieces :: S.Set Subpiece
  } deriving Show
makeLenses ''Peer

data Loader = Loader 
  { _storage :: Storage 
  , _torrent :: T.Torrent
  , _peers :: M.Map PeerId Peer
  , _knownPeers :: S.Set NS.SockAddr
  , _loadedPieces :: S.Set Word32
  , _requestedSubpieces :: S.Set Subpiece
  , _rng :: StdGen
  } deriving Show
makeLenses ''Loader

data Incoming 
  = InMessage PeerId PeerMessage
  | PeerDied PeerId
  | Heartbeat
  | Connected PeerId NS.SockAddr 
  | ConnectionFailed NS.SockAddr 
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

batchSize :: Int 
batchSize = 3
{-
for(let i = 0; i < arr.length; i ++) {
  swap(arr[i], random(i, arr.length));
}

-} 


shuffle :: forall a. Ord a => [a] -> L [a] 
shuffle x = do
  r <- use rng
  
  let 
    swap arr i j = do
      ai <- A.readArray arr i
      aj <- A.readArray arr j
      
      A.writeArray arr j ai
      A.writeArray arr i aj


    action :: forall s. ST s ([a], StdGen)
    action = do
      rngRef <- newSTRef r
      (arr :: A.STArray s Int a) <- A.newListArray (0, length x - 1) x
      
      let max = length x - 1
      forM [0..max] $ \i -> do
        rng <- readSTRef rngRef
        let (j, rng') = randomR (i, max - 1) rng
        writeSTRef rngRef rng'

        swap arr i j

      x' <- A.getElems arr
      r' <- readSTRef rngRef
      return (x', r')

    (x', r') = runST action

  assertM (S.fromList x == S.fromList x')
    "Shuffle is not shuffling"

  rng .= r'
  return x'

selectSubpieces :: PeerId -> Word32 -> L [Word32]
selectSubpieces pid index = do
  t <- use torrent
  loaded <- use loadedPieces
  requested <- use requestedSubpieces
  peerRequested <- use $ peers.(ix pid).queuedSubpieces
  stored <- storedSubpieces index <$> use storage
      
  let
    endgame = length (t^.T.pieces) - S.size loaded < 10
    subpiecesCount = fromIntegral $ subpiecesInPiece t index
    range = [0..subpiecesCount - 1]
    
    selector sidx = 
      not (sidx `S.member` stored) && 
        if endgame
          then 
             not (Subpiece index sidx `S.member` peerRequested)
          else
             not (Subpiece index sidx `S.member` requested)
  
  shuffled <- shuffle range

  return
    $ take batchSize 
    $ filter selector
    $ shuffled

assertM :: Monad m => Bool -> String -> m () 
assertM cond msg = unless cond $ error msg

tryToRequestPiece :: PeerId -> L ()
tryToRequestPiece pid = do
  t <- use torrent
  s <- use storage

  loaded <- use loadedPieces
  requested <- use requestedSubpieces
  peerRequested <- use $ peers.(ix pid).queuedSubpieces
  
  ps <- use peers
  peerPieces <- use $ peers.(ix pid).pieces
  
  let 
    cached = storedPieces s
    candidates = 
      let 
        available = peerPieces `S.difference` loaded
        priority = available `S.intersection` cached
      in
        if S.size priority > 10 + 5 * (M.size ps) 
          then priority
          else available

  assertM (S.size (loaded `S.intersection` cached) == 0)
    (show loaded <> "\n" <> show cached) 

  when (S.size peerRequested < 20 && S.size candidates /= 0) $ do
    selectedIdx <- zoom rng $ state $ randomR (0, S.size candidates - 1)
    let pieceIdx = S.elemAt selectedIdx candidates

    subpieceIdxs <- selectSubpieces pid pieceIdx
    
    tell
      $ map (OutMessage pid)
      $ map 
          (\subpieceIdx -> 
              Request 
                pieceIdx 
                (subpieceIdx * subpieceLength) 
                subpieceLength) 
      
      $ subpieceIdxs

    let 
      subpieceSet = S.fromList $ map (Subpiece pieceIdx) subpieceIdxs

    requestedSubpieces %= (<> subpieceSet)
    peers.(ix pid).queuedSubpieces %= (<> subpieceSet)

loader :: Incoming -> L ()
loader inMsg = 
  case inMsg of
    Heartbeat -> do
      ps <- use peers
      
      forM_ ps $ \p -> do
        unless (p^.choked) $ 
          tryToRequestPiece (p^.pid)

      kps <- use knownPeers

      when (length kps < 10) $ do
        tell [AskDhtPeers, AnnounceTracker]

      when (M.size ps < 20) $ do
        let
          candidates = S.difference kps (S.fromList $ map (view addr) $ M.elems ps)
          n = 10 - M.size ps
        
        when (S.size candidates > 0) $ do
          (indexes :: [Int]) <- zoom rng $ replicateM n $ state $ randomR (0, S.size candidates - 1)
          
          let 
            selected = map (`S.elemAt` candidates) $ nub indexes

          tell $ map Connect selected

    PeerDied pid -> do
      p <- use peers
      peers %= (M.delete pid)
      requestedSubpieces %= (`S.difference` (p^.(ix pid).queuedSubpieces))      
    
    ConnectionFailed addr -> do
      knownPeers %= (S.delete addr) 

    DhtPeers peers -> do
      knownPeers %= (<> S.fromList peers)
    TrackerPeers peers -> do
      knownPeers %= (<> S.fromList peers)
    FsResponse pid index offset bytes -> do
      tell $ [OutMessage pid $ Piece index offset bytes]
    
    Connected pid addr  -> do
      peers %= (M.insert pid $ Peer {
        _pid = pid,
        _addr = addr,
        _rating = 0,
        _choked = True,
        _interested = False,
        _pieces = mempty,
        _queuedSubpieces = mempty 
      })
      
      p <- use (torrent.T.pieces)
      loaded <- use loadedPieces
      tell $ [
          OutMessage pid $ BitField $ map (`S.member` loaded) $ A.indices p,
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
          peers.(ix pid).choked .= True
        UnChoke -> do
          peers.(ix pid).choked .= False
          tryToRequestPiece pid
            
        Interested -> do
          peers.(ix pid).interested .= True
        NotInterested -> do
          peers.(ix pid).interested .= False
        BitField flags -> do
          t <- use torrent
          let flags' = take (length $ t^.T.pieces) flags 
          peers.(ix pid).LoaderV2.pieces .= 
            (S.fromList $ [index | (index, flag) <- zip [0 ..] flags', flag]) 
        
        Have piece -> do
          peers.(ix pid).LoaderV2.pieces %= (S.insert piece)
          ps <- use peers
          
          unless ((ps M.! pid)^.choked) $ 
            tryToRequestPiece (pid)

        Request index offset length -> do
          when (length == subpieceLength) $ do
            tell $ [GetFs pid index offset length]
        
        Piece pieceIdx offset payload -> do
          loaded <- use loadedPieces
          let 
            offsetIsCorrect = offset `mod` subpieceLength == 0
            sizeIsCorrect = B.length payload == fromIntegral subpieceLength
            subpieceIdx = offset `div` subpieceLength 
            subpiece = Subpiece pieceIdx subpieceIdx

          when (offsetIsCorrect && sizeIsCorrect) $ do
            requestedSubpieces %= (S.delete subpiece)
            peers.(ix pid).queuedSubpieces %= (S.delete subpiece)

            ps <- use peers

            unless ((ps M.! pid)^.choked) $ do
              tryToRequestPiece pid  

            unless (pieceIdx `S.member` loaded) $ do
              outcome <- zoom storage (state $ store subpiece payload)
              case outcome of
                Stored -> return ()
                Failed -> return ()
                Finished piece -> do
                  tell [PieceLoaded (fromIntegral pieceIdx) piece]

                  let msg = Have pieceIdx
                  tell $ map (\pid -> OutMessage pid msg) (M.keys ps)

                  loadedPieces %= (S.insert pieceIdx)

run :: L a -> Loader -> (Loader, [Outgoing], a)
run l state = (loader, events, a) where
  ((a, loader), events) = runWriter (runStateT l state) 

startLoader :: T.Torrent -> S.Set Word32 -> STM.TChan Incoming -> STM.TChan Outgoing ->  IO ()
startLoader torrent loaded incoming outgoing = do
  rng <- getStdGen
  stateRef <- newIORef (Loader {
    _storage = mkStorage torrent,
    _torrent = torrent,
    _peers = mempty,
    _knownPeers =  mempty,
    _loadedPieces = loaded,
    _requestedSubpieces = mempty,
    _rng = rng
  })

  forever $ do
    inMsg <- STM.atomically $ STM.readTChan incoming
    -- putStrLnPar $ take 100 $ show inMsg

    state <- readIORef stateRef
    -- putStrPar $ mconcat $ map ((<> "\n") . show) $ S.toList (state^.requestedSubpieces)
    -- printPar $ map (M.keys <$>) $ M.toList (state^.storage.Storage.mapping)

    let (state', events, ()) = run (loader inMsg) state
    -- mapM (putStrLnPar . take 100 . show) events

    writeIORef stateRef state' 
    
    STM.atomically $ do
      mapM (STM.writeTChan outgoing) events