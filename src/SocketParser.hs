{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module SocketParser where

import Control.Lens
import Control.Monad.State
import Data.ByteString as B
import Data.IORef
import Data.Word
import GHC.Base
import Network.Simple.TCP
import Parser

data SocketParserState = SocketParserState
  { _socket :: Socket,
    _bytestringRef :: IORef ByteString,
    _pos :: Int
  }

makeLenses ''SocketParserState

type SocketParser = StateT SocketParserState IO

-- deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail)

instance Parser Word8 SocketParser where
  next = do
    ch <- peek
    modify (over pos (+ 1))

    return ch

  peek = do
    idx <- gets ((+ 1) . view pos)
    ref <- gets $ view bytestringRef
    bstr <- liftIO (readIORef ref)

    if B.length bstr > idx
      then return $ B.index bstr idx
      else do
        sock <- gets (view socket)
        maybeBytes <- recv sock (2 ^ 16)
        bytes <- maybeBytes `orFail` "End of input"

        let newbstr = bstr <> bytes
        liftIO $ writeIORef ref (newbstr)
        -- liftIO $ print newbstr
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

-- cutParsed :: SocketParserState -> IO SocketParserState
-- cutParsed state = do
--   bstr <- readIORef (state ^. bytestringRef)
--   pos <- state ^. pos

--   newRef <- newIORef $ B.drop bstr pos
