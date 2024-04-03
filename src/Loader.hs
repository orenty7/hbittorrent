{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Loader (
  Connection (..),
  init,
  socket,
  chocked,
  interested,
  torrent,
  Loader.pieces,
  stateRef,
  queuedPieces,
  events,
  globalState,
  --
  LoaderState (..),
  connections,
  subpieceStorage,
  piecesToLoad,
  finishedPieces,
  lock,
  randomGen,
  --
  performHandshake,
  react,
  buildMessage,
) where

import Peer (Handshake, buildHandshake, decodeHandshake)
import Protocols (Message (..), PeerMessage (..), Serializable (..))
import Torrent (Torrent, fileLength, name, piece, pieceLength, pieces)
import Utils (convert)

import qualified SocketParser as SP

import qualified Control.Concurrent.STM as STM
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Network.Simple.TCP as TCP
import qualified System.IO as IO

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Maybe
import qualified Data.Set as S
import qualified Data.Word as W

import Control.Exception (onException)
import Control.Lens (makeLenses, over, set, view, (&))
import Control.Monad (forM_, unless, void, when)
import Control.Monad.State (evalStateT)
import Data.IORef (IORef, readIORef, writeIORef)

import Control.Monad.Writer (execWriter)
import System.Random (StdGen, randomR)

import Prelude as P hiding (init)

type PieceIndex = W.Word32
type SubpieceIndex = W.Word32

data Event = Finished W.Word32 deriving (Show)

data Connection = Connection
  { _socket :: TCP.Socket
  , _chocked :: Bool
  , _interested :: Bool
  , _torrent :: Torrent.Torrent
  , _pieces :: S.Set PieceIndex
  , _stateRef :: IORef SP.SocketParserState
  , _queuedPieces :: M.Map PieceIndex ()
  , _events :: STM.TChan Event
  , _globalState :: STM.TVar LoaderState
  }

data LoaderState = LoaderState
  { _connections :: [Connection]
  , _subpieceStorage :: M.Map PieceIndex (M.Map SubpieceIndex B.ByteString)
  , _piecesToLoad :: S.Set PieceIndex
  , _finishedPieces :: S.Set PieceIndex
  , _lock :: STM.TMVar ()
  , _randomGen :: StdGen
  }

makeLenses ''Connection

makeLenses ''LoaderState

subpieceSize :: (Integral a) => a
subpieceSize = 2 ^ (14 :: Integer)

log :: Connection -> String -> IO ()
log connection message = do
  state <- STM.atomically $ STM.readTVar (view globalState connection)
  let l = view lock state

  STM.atomically $ STM.takeTMVar l
  putStr message
  IO.hFlush IO.stdout
  STM.atomically $ STM.putTMVar l ()

performHandshake :: IORef SP.SocketParserState -> TCP.Socket -> Handshake -> IO Handshake
performHandshake stateRef socket handshake = do
  TCP.send socket $ buildHandshake handshake
  SP.runParser stateRef decodeHandshake

init :: TCP.Socket -> Torrent -> IORef SP.SocketParserState -> STM.TChan Event -> STM.TVar LoaderState -> Connection
init socket torrent stateRef events globalState =
  Connection socket False False torrent mempty stateRef mempty events globalState

requeuePieces :: Connection -> IO ()
requeuePieces connection = do
  let indexes = M.keys (view queuedPieces connection)
  STM.atomically $ do
    STM.modifyTVar (view globalState connection) (over piecesToLoad (S.union $ S.fromList indexes))

  return ()

calcSubpiecesInPiece :: (Integral a) => Connection -> PieceIndex -> a
calcSubpiecesInPiece connection index =
  let isLast =
        index
          == ( connection
                & view (torrent . Torrent.pieces)
                & B.length
                & (\x -> x `div` 20 - 1)
                & convert
             )
      currentPieceLength =
        if isLast
          then view (torrent . fileLength) connection `mod` view (torrent . pieceLength) connection
          else view (torrent . pieceLength) connection
   in fromInteger $ (currentPieceLength + subpieceSize - 1) `div` subpieceSize

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

  let subpieces = Data.Maybe.mapMaybe (`M.lookup` subpieceMap) [0 .. calcSubpiecesInPiece connection index - 1]

  if P.length subpieces == calcSubpiecesInPiece connection index
    then
      let piece = mconcat subpieces
          hash = view (torrent . Torrent.piece (convert index)) connection
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

buildMessage :: PeerMessage -> B.ByteString
buildMessage peerMessage =
  peerMessage
    & serialize
    & execWriter
    & (\case [message :: Message] -> message; _ -> error "Incorrect serialization")
    & serialize
    & execWriter
    & B.pack

react :: IORef Connection -> IO ()
react connectionRef = do
  connection <- readIORef connectionRef

  flip onException (requeuePieces connection) $ do
    (message :: Message) <- SP.runParser (view stateRef connection) parse
    peerMessage <- evalStateT parse [message]

    case peerMessage of
      Cancel{} ->
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
      BitField flags -> do
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
              finished <- STM.atomically $ do
                STM.writeTChan (view events connection) $ Finished index
                STM.modifyTVar (view globalState connection) (over finishedPieces (S.insert index))

                state <- STM.readTVar (view globalState connection)
                return $ S.size (view finishedPieces state)

              writeIORef connectionRef (over queuedPieces (M.delete index) connection)

              IO.withFile (view (torrent . name) connection) IO.ReadWriteMode $ \handle -> do
                let offset = convert index * view (torrent . pieceLength) connection
                IO.hSeek handle IO.AbsoluteSeek offset
                B.hPut handle piece
              let total =
                    connection
                      & view (torrent . Torrent.pieces)
                      & B.length
                      & (`div` 20)
              Loader.log connection $ "\27[2\27[1G" <> "Loading (" <> show finished <> "/" <> show total <> ")"

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
        let toLoad = (view piecesToLoad state)
        let peerHas = (view Loader.pieces connection)
        let candidates = S.intersection toLoad peerHas

        if S.size candidates == 0
          then return Nothing
          else do
            let range = (0, S.size candidates - 1)
            let (index, randomGen') = randomR range (view randomGen state)
            let piece = S.elemAt index candidates
            STM.writeTVar
              (view globalState connection)
              ( state
                  & (over piecesToLoad (S.delete piece))
                  & (set randomGen randomGen')
              )

            return $ Just piece

      case pieceToRequest of
        Nothing -> when (M.null (view queuedPieces connection)) $ do
          fail "Done"
        Just index -> do
          writeIORef connectionRef (over queuedPieces (M.insert index ()) connection)
          let subpiecesInPiece = calcSubpiecesInPiece connection index

          forM_ [0 .. subpiecesInPiece - 1] $ \subindex -> do
            let message = buildMessage $ Request index (subindex * subpieceSize) subpieceSize
            TCP.send (view Loader.socket connection) $ message

      return ()

    let chan = view events connection

    let processEvents fn = do
          maybeEvent <- STM.atomically $ STM.tryReadTChan chan
          case maybeEvent of
            Nothing -> return ()
            Just event -> do
              void $ fn event
              processEvents fn

    processEvents $ \case
      Finished piece -> do
        TCP.send (view Loader.socket connection) $ buildMessage $ Have piece
