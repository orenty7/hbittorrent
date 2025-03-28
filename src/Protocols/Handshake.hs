{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Protocols.Handshake (
  Handshake (..),
  handshakeSize
) where

import Parser.Core
import Protocols.Serializable (Serializable (..), Serializer)
import qualified Hash as H

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B


import Control.Lens (makeLenses, view, (^.))
import Control.Monad.Writer


data Handshake = Handshake
  { _extensionFlags :: [Bool]
  , _infoHash :: H.Hash
  , _peerId :: B.ByteString
  }
  deriving (Eq, Show)
makeLenses ''Handshake


handshakeBytes :: B.ByteString
handshakeBytes = B.pack $ (19 :) $ B.unpack "BitTorrent protocol"

handshakeSize :: Int
handshakeSize = 1 + 19 + 8 + 20 + 20

instance Serializable Handshake where
  serialize :: Handshake -> Serializer ()
  serialize handshake = do
    tell $ B.byteString handshakeBytes
    serialize $ view extensionFlags handshake
    tell $ B.byteString $ H.unHash $ handshake^.infoHash
    tell $ B.byteString $ handshake^.peerId
    
  parse :: Parser Handshake
  parse = do
    mapM expectByte $ B.unpack handshakeBytes

    packedFlags <- nextN 8
    extensionFlags <- case runParser parse packedFlags of
      Just result -> return result
      Nothing -> fail "Can't parse extension flags"

    infoHash <- H.mkHash <$> nextN 20
    peerId <- nextN 20

    return $ Handshake extensionFlags infoHash peerId