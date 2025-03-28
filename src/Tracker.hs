{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tracker (
  Announce (..),
  interval,
  peers,
  --
  getPeers,
) where

import Bencode hiding (encode)
import Torrent (Torrent, fileLength, infoHash)
import Hash (unHash)
import Utils (orFail, thenFail)

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU
import qualified Network.Socket as Socket


import Control.Lens
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.UTF8 (fromString)
import Data.Word (Word32)
import Network.HTTP.Client (Response (responseBody))
import Network.HTTP.Simple
import Text.URI (URI, renderStr)

import Prelude as P

type Port = Word32

data Announce = Announce
  { _interval :: Integer
  , _peers :: [Socket.SockAddr]
  }

makeLenses ''Announce

buildQuery :: Torrent -> Port -> B.ByteString -> Query
buildQuery torrent port peerId =
  P.map
    (Just <$>)
    [ ("info_hash", unHash (torrent ^. infoHash))
    , ("peer_id", peerId)
    , ("port", fromString $ show port)
    , ("uploaded", fromString "0")
    , ("downloaded", fromString "0")
    , ("left", fromString $ show (torrent ^. fileLength))
    ]

extractPeer :: (MonadFail m, MonadIO m) => Bencode -> m [Socket.SockAddr]
extractPeer bencode = do
  bmap <-
    (bencode ^? _BDict)
      `orFail` "Bencode is not a dict"

  host <-
    (bmap ^? ix "ip" . _BString)
      `orFail` "Can't extract IP"

  port <-
    (bmap ^? ix "port" . _BInteger)
      `orFail` "Can't extract port"

  addrs <- liftIO $ Socket.getAddrInfo Nothing (Just $ BSU.toString host) (Just $ show port)
  return $ P.map (Socket.addrAddress) addrs

createRequestAnnounce :: (MonadFail m) => Torrent -> URI -> Port -> B.ByteString -> m Request
createRequestAnnounce torrent trackerUri port peerId = do
  request <-
    (parseRequest $ renderStr $ trackerUri)
      `orFail` "Can't reparse or announce is not present request"

  return $ addToRequestQueryString (buildQuery torrent port peerId) request

parseResponse :: (MonadIO m, MonadFail m) => Bencode -> m Announce
parseResponse bencode = do
  interval <-
    (bencode ^? _BDict . ix "interval" . _BInteger)
      `orFail` "Can't extract interval from response"

  bencodedPeers <-
    (bencode ^? _BDict . ix "peers" . _BList)
      `orFail` "Response doesn't have peers"

  (Announce interval) . mconcat <$> mapM extractPeer bencodedPeers

getPeers :: (MonadIO m, MonadFail m) => Torrent -> URI -> Port -> m Announce
getPeers torrent trackerUri port = do
  request <- createRequestAnnounce torrent trackerUri port "bsdfbsdfbsdfbsdfbsdf"
  response <- httpBS request
  bencode <- parse $ responseBody response

  (bencode ^? _BDict . ix "failure")
    `thenFail` \err -> "Response contains failure " <> show err

  parseResponse bencode
