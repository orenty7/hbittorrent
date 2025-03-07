{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHC2021 #-}

module LoaderV2 (startLoader) where

import Torrent
import Protocols (PeerMessage (..))
import Storage

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Network.Socket as NS

import Control.Lens
import Data.IORef
import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

type PeerId = Int
data Peer = Peer 
  { _id :: PeerId
  , _addr :: NS.SockAddr
  , _rating :: Int
  , _choke :: Bool
  , _interested :: Bool
  , _pieces :: S.Set Word32
  , _queuedSubpieces :: [Int]
  } deriving Show
makeLenses ''Peer


data Loader = Loader 
  { _storage :: Storage 
  , _torrent :: Torrent
  , _peers :: M.Map PeerId Peer
  , _knownPeers :: S.Set NS.SockAddr
  } deriving Show
makeLenses ''Loader

data Incoming 
  = InMessage PeerId PeerMessage
  | PeerDied PeerId
  | Heartbeat
  | Connected PeerId
  | DhtPeers [NS.SockAddr]

data Outgoing 
  = OutMessage PeerId PeerMessage
  | PieceLoaded Int B.ByteString
  | Connect NS.SockAddr
  | AskDhtPeers

type L = StateT Loader (Writer [Outgoing])

loader :: Incoming -> L ()
loader inMsg = 
  case inMsg of
    Heartbeat -> do
      kp <- use knownPeers
      when (length kp < 10) $ do
        tell [AskDhtPeers]
      
      p <- use peers      
      when (M.size p < 10) $ do
        let candidates = S.toList $ S.difference kp (S.fromList $ map (view addr) $ M.elems p)
        case candidates of
          (x:_) -> tell [Connect x]
          _ -> return () 

    PeerDied pid -> 
      peers %= (M.delete pid)

    InMessage pid msg ->
      case msg of
        Cancel{} ->
          return ()
        KeepAlive ->
          return ()
        Choke ->
          peers.(ix pid).choke .= True
        UnChoke ->
          peers.(ix pid).choke .= False
        Interested ->
          peers.(ix pid).interested .= True
        NotInterested ->
          peers.(ix pid).interested .= False
        BitField flags -> do
          peers.(ix pid).LoaderV2.pieces .= 
            (S.fromList $ [index | (index, flag) <- zip [0 ..] flags, flag]) 
        Have piece ->
          peers.(ix pid).LoaderV2.pieces %= (S.insert piece)
        Request index offset length -> do
          return ()
        Piece pieceIdx offset payload -> do
          let 
            offsetIsCorrect = offset `mod` (fromIntegral subpieceLength) == 0
            sizeIsCorrect = B.length payload == subpieceLength
            subpieceIdx = (fromIntegral offset) `div` subpieceLength 
            subpiece = Subpiece (fromIntegral pieceIdx) subpieceIdx payload

          when (offsetIsCorrect && sizeIsCorrect) $ do
            outcome <- zoom storage (state $ store subpiece)

            case outcome of
              Stored -> return ()
              Failed -> return () 
              Finished piece -> 
                tell [PieceLoaded (fromIntegral pieceIdx) piece]

            --   addSubpiece connection index offset subpiece
            --   tryToBuildPiece connection index

            -- case maybePiece of
            --   Nothing -> return ()
            --   Just piece -> do
            --     finished <- STM.atomically $ do
            --       STM.writeTChan (view events connection) $ Finished index
            --       STM.modifyTVar (view globalState connection) (over finishedPieces (S.insert index))

            --       state <- STM.readTVar (view globalState connection)
            --       return $ S.size (view finishedPieces state)

            --     writeIORef connectionRef (over queuedPieces (M.delete index) connection)

            --     IO.withFile (view (torrent . name) connection) IO.ReadWriteMode $ \handle -> do
            --       let offset = convert index * view (torrent . pieceLength) connection
            --       IO.hSeek handle IO.AbsoluteSeek offset
            --       B.hPut handle piece
            --     let total =
            --           length (view (torrent . Torrent.pieces) connection)
            --     Loader.log connection $ "\27[2\27[1G" <> "Loading (" <> show finished <> "/" <> show total <> ")"

run :: L a -> Loader -> (Loader, [Outgoing], a)
run l state = (loader, events, a) where
  ((a, loader), events) = runWriter (runStateT l state) 

startLoader :: Torrent -> STM.TChan Incoming -> STM.TChan Outgoing ->  IO ()
startLoader torrent incoming outgoing = do
  let storage = mkStorage torrent
  stateRef <- newIORef (Loader {
    _storage = storage,
    _torrent = torrent,
    _peers = mempty,
    _knownPeers =  mempty
  })

  forever $ do
    inMsg <- STM.atomically $ STM.readTChan incoming
    state <- readIORef stateRef
    
    let (state', events, ()) = run (loader inMsg) state

    writeIORef stateRef state' 
    STM.atomically $ do
      mapM (STM.writeTChan outgoing) events