{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Loader where

import qualified Control.Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Exception
import Control.Lens (makeLenses, over, set, view, (&), (^.))
import Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Map as M
import qualified Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Word as W
import qualified Network.Simple.TCP as TCP
import Peer
import SocketParser as SP
import qualified System.IO as IO
import Torrent
import Tracker
import Prelude as P

data Event = Finished W.Word32 deriving (Show)

type PieceIndex = W.Word32

type SubpieceIndex = W.Word32

data GlobalState = GlobalState
  { _subpieceStorage :: M.Map PieceIndex (M.Map SubpieceIndex B.ByteString),
    _piecesToLoad :: S.Set PieceIndex,
    _lock :: STM.TMVar ()
  }

makeLenses ''GlobalState

data Connection = Connection
  { _socket :: TCP.Socket,
    _chocked :: Bool,
    _interested :: Bool,
    _torrent :: Torrent.Torrent,
    _pieces :: S.Set PieceIndex,
    _stateRef :: IORef SP.SocketParserState,
    _queuedPieces :: M.Map PieceIndex (),
    _globalEvents :: STM.TChan Event,
    _globalState :: STM.TVar GlobalState
  }

makeLenses ''Connection

data LoaderState = LoaderState
  { _connections :: [Connection]
  }

makeLenses ''LoaderState

subpieceSize :: (Integral a) => a
subpieceSize = 2 ^ 14

-- type Loader = StateT LoaderState IO
type PeerId = B.ByteString

log :: Connection -> String -> IO ()
log connection message = do
  state <- STM.atomically $ STM.readTVar (view globalState connection)
  let l = view lock state

  STM.atomically $ STM.takeTMVar l
  putStrLn message
  STM.atomically $ STM.putTMVar l ()

convert :: (Integral a, Integral b) => a -> b
convert = fromInteger . toInteger

performHandshake :: IORef SocketParserState -> TCP.Socket -> Handshake -> IO PeerId
performHandshake stateRef socket handshake = do
  TCP.send socket $ buildHandshake handshake
  response <- runParser stateRef decodeHandshake

  when (view Peer.infoHash response /= view Peer.infoHash handshake) $ do
    fail "Infohashes doesn't match"

  return $ handshake ^. peerId

init :: TCP.Socket -> Torrent -> IORef SocketParserState -> STM.TChan Event -> STM.TVar GlobalState -> Connection
init socket torrent stateRef globalEvents globalState =
  Connection socket False False torrent mempty stateRef mempty globalEvents globalState

requeuePieces :: Connection -> IO ()
requeuePieces connection = do
  let indexes = M.keys (view queuedPieces connection)
  STM.atomically $ do
    STM.modifyTVar (view globalState connection) (over piecesToLoad (S.union $ S.fromList indexes))

  return ()

calcSubpiecesInPiece :: (Integral a) => Connection -> a
calcSubpiecesInPiece connection = fromInteger $ view (torrent . pieceLength) connection `div` subpieceSize

addSubpiece :: Connection -> W.Word32 -> W.Word32 -> B.ByteString -> STM.STM ()
addSubpiece connection index offset subpiece = do
  state <- STM.readTVar (view globalState connection)

  let subindex = offset `div` subpieceSize

  let pieceMap = view subpieceStorage state
  let subpieceMap = M.findWithDefault mempty index pieceMap

  let subpieceMap' = M.insert subindex subpiece subpieceMap
  let pieceMap' = M.insert index subpieceMap' pieceMap

  STM.writeTVar (view globalState connection) (set subpieceStorage pieceMap' state)

tryToBuildPiece :: Connection -> PieceIndex -> STM.STM (Maybe B.ByteString)
tryToBuildPiece connection index = do
  state <- STM.readTVar (view globalState connection)
  let pieceMap = view subpieceStorage state
  let subpieceMap = M.findWithDefault mempty index pieceMap

  let subpieces = Data.Maybe.mapMaybe (`M.lookup` subpieceMap) [0 .. calcSubpiecesInPiece connection - 1]

  if P.length subpieces == calcSubpiecesInPiece connection
    then
      let piece = mconcat subpieces
          hash = view (torrent . Torrent.pieces) connection !! convert index
       in if SHA1.hash piece /= hash
            then do
              STM.writeTVar
                (view globalState connection)
                (state & over subpieceStorage (M.delete index)) -- Have to redownload all subpieces
              return Nothing
            else do
              STM.writeTVar
                (view globalState connection)
                ( state
                    & over subpieceStorage (M.delete index)
                    & over piecesToLoad (S.delete index)
                )

              return $ Just piece
    else do
      return Nothing

react :: IORef Connection -> IO ()
react connectionRef = do
  connection <- readIORef connectionRef

  flip onException (requeuePieces connection) $ do
    message <- runParser (view stateRef connection) (messageDecoder $ view torrent connection)

    let str = show message
    Loader.log connection $
      if P.length str > 100
        then P.reverse $ P.dropWhile (/= ',') $ P.reverse $ P.take 100 str
        else str

    case message of
      Cancel {} ->
        return ()
      KeepAlive ->
        return ()
      Choke ->
        writeIORef connectionRef (set chocked True connection)
      Unchoke ->
        writeIORef connectionRef (set chocked False connection)
      Interested ->
        writeIORef connectionRef (set interested True connection)
      NotInterested ->
        writeIORef connectionRef (set interested False connection)
      Bitfield flags ->
        writeIORef connectionRef (set Loader.pieces (S.fromList $ [index | (index, flag) <- P.zip [0 ..] flags, flag]) connection)
      Have piece ->
        writeIORef connectionRef (over Loader.pieces (S.insert piece) connection)
      Piece index offset subpiece -> do
        let offsetIsCorrect = offset `mod` subpieceSize == 0
        let sizeIsCorrect = B.length subpiece == subpieceSize
        when (offsetIsCorrect && sizeIsCorrect) $ do
          maybePiece <- STM.atomically $ do
            addSubpiece connection index offset subpiece
            tryToBuildPiece connection index

          case maybePiece of
            Nothing -> return ()
            Just piece -> do
              STM.atomically $ STM.writeTChan (view globalEvents connection) $ Finished index
              writeIORef connectionRef (over queuedPieces (M.delete index) connection)

              IO.withFile (T.unpack $ view (torrent . name) connection) IO.ReadWriteMode $ \handle -> do
                let offset = convert index * view (torrent . pieceLength) connection
                IO.hSeek handle IO.AbsoluteSeek offset
                B.hPut handle piece
      --
      Request index offset length -> do
        maybePiece <- STM.atomically $ do
          -- state <- readTVar $ view globalState connection
          -- let pieces = view loadedPieces state
          -- return $ M.lookup index pieces
          return Nothing

        case maybePiece of
          Nothing -> return ()
          Just piece -> do
            let bytes = piece & B.drop (convert offset) & B.take (convert length)
            let encoded = buildMessage $ Piece index offset bytes
            TCP.send (view Loader.socket connection) encoded

    connection <- readIORef connectionRef

    unless ((view chocked connection) || M.size (view queuedPieces connection) > 5) $ do
      pieceToRequest <- STM.atomically $ do
        state <- STM.readTVar (view globalState connection)

        if S.size (view piecesToLoad state) == 0
          then return Nothing
          else do
            let (piece, rest) = S.deleteFindMin (view piecesToLoad state)
            STM.writeTVar (view globalState connection) (set piecesToLoad rest state)

            return $ Just piece

      case pieceToRequest of
        Nothing -> return ()
        Just index -> do
          writeIORef connectionRef (over queuedPieces (M.insert index ()) connection)

          let subpiecesInPiece = calcSubpiecesInPiece connection
          forM_ [0 .. subpiecesInPiece - 1] $ \subindex -> do
            let message = buildMessage $ Request index (subindex * subpieceSize) subpieceSize
            TCP.send (view Loader.socket connection) $ message

      return ()

    let chan = view globalEvents connection

    let processEvents fn = do
          maybeEvent <- STM.atomically $ STM.tryReadTChan chan
          case maybeEvent of
            Nothing -> return ()
            Just event -> do
              fn event
              processEvents fn

    processEvents $ \case
      Finished piece -> do
        Loader.log connection $ "finished piece " <> show piece
        TCP.send (view Loader.socket connection) $ buildMessage $ Have piece

-- Piece index begin length
-- Cancel
-- Request
