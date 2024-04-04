{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser.Socket (SocketParser, SocketParserState (..), init, runParser) where

import Parser.Core (Parser (..))
import Utils (orFail)

import qualified Data.ByteString as B

import Control.Lens (makeLenses, over, view)
import Control.Monad.State (MonadIO (liftIO), StateT (runStateT), gets, modify)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Network.Simple.TCP (Socket, recv)

import Prelude hiding (init)

type SocketParser = StateT SocketParserState IO

data SocketParserState = SocketParserState
  { _socket :: Socket
  , _bytestringRef :: IORef B.ByteString
  , _pos :: Int
  }

makeLenses ''SocketParserState

instance Parser Word8 SocketParser where
  eof :: SocketParser Bool
  eof = return False -- Doesn't matter for the SocketParser rn

  next :: SocketParser Word8
  next = do
    ch <- peek
    modify (over pos (+ 1))

    return ch

  peek :: SocketParser Word8
  peek = do
    idx <- gets ((+ 1) . view pos)
    ref <- gets $ view bytestringRef
    bstr <- liftIO (readIORef ref)

    if B.length bstr > idx
      then return $ B.index bstr idx
      else do
        sock <- gets (view socket)
        maybeBytes <- recv sock (2 ^ (14 :: Integer))
        bytes <- maybeBytes `orFail` "End of input"

        let newbstr = bstr <> bytes
        liftIO $ writeIORef ref newbstr
        return $ B.index newbstr idx

init :: Socket -> IO SocketParserState
init socket = do
  ref <- newIORef B.empty
  return $ SocketParserState socket ref (-1)

runParser :: IORef SocketParserState -> SocketParser t -> IO t
runParser stateRef parser = do
  state <- readIORef stateRef
  (parsed, SocketParserState socket ref pos) <- runStateT parser state

  bstr <- readIORef ref
  newRef <- newIORef $ B.drop (pos + 1) bstr

  writeIORef stateRef (SocketParserState socket newRef (-1))

  return parsed
