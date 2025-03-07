{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHC2021 #-}

module LoaderV2 (startLoader) where

import Torrent
import Protocols (PeerMessage (..))
import Storage

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as B
import qualified Data.Map as M

import Control.Lens
import Data.IORef
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

type PeerId = Int
data Peer = Peer 
  { _id :: PeerId
  , _rating :: Int
  , _choke :: Bool
  , _interested :: Bool
  , _queuedSubpieces :: [Int]
  } deriving Show
makeLenses ''Peer


data Loader = Loader 
  { _storage :: Storage 
  , _torrent :: Torrent
  } deriving Show
makeLenses ''Loader

data Incoming 
  = InMessage PeerId PeerMessage
  | PeerDied PeerId
  | Heartbeat

data Outgoing 
  = OutMessage PeerId PeerMessage
  | PieceLoaded Int B.ByteString


type L = StateT Loader (Writer [Outgoing])

loader :: Incoming -> L ()
loader _ = return () 


run :: L a -> Loader -> (Loader, [Outgoing], a)
run l state = (loader, events, a) where
  ((a, loader), events) = runWriter (runStateT l state) 


startLoader :: Torrent -> STM.TChan Incoming -> STM.TChan Outgoing ->  IO ()
startLoader torrent incoming outgoing = do
  let storage = mkStorage torrent
  stateRef <- newIORef (Loader storage torrent)

  forever $ do
    inMsg <- STM.atomically $ STM.readTChan incoming
    state <- readIORef stateRef
    
    let (state', events, _) = run (loader inMsg) state

    writeIORef stateRef state' 
    STM.atomically $ do
      mapM (STM.writeTChan outgoing) events