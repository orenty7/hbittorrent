{-# LANGUAGE GHC2021 #-}

module Parser.Array (ArrayParser (..), runParser) where

import Parser.Core (Parser (..))

import Data.Array qualified as A

import Control.Applicative (Alternative)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get), StateT, evalStateT, modify')

type Memory sym = A.Array Int sym
type Position = Int

newtype ArrayParser sym a = ArrayParser (ReaderT (Memory sym) (StateT Position Maybe) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Memory sym)
    , MonadState Position
    , MonadFail
    , Alternative
    )

instance Parser sym (ArrayParser sym) where
  next :: ArrayParser sym sym
  next = do
    sym <- peek
    modify' (+ 1)
    return sym

  peek :: ArrayParser sym sym
  peek = do
    isEof <- eof

    when isEof $ do
      fail "End of input"

    pos <- get
    asks (A.! pos)

  eof :: ArrayParser sym Bool
  eof = do
    pos <- get
    range <- asks A.bounds

    return $ not $ A.inRange range pos

runParser :: ArrayParser sym a -> Memory sym -> Maybe a
runParser (ArrayParser parser) memory = evalStateT (runReaderT parser memory) 0