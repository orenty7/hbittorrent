{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tracker where

-- import Network.Simple.TCP

import Bencode hiding (encode)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString as B
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Data.Word
import Network.HTTP.Client (Response (responseBody))
import Network.HTTP.Simple
import Network.Simple.TCP (HostName)
import Parser
import Text.URI
import Torrent (Torrent, announce, fileLength, infoHash)
import Prelude as P

import Control.Exception (bracket)
import qualified Data.ByteString.UTF8 as BSU
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBs

type Port = Word32

data Announce = Announce
  { _interval :: Integer
  , _peers :: [Socket.SockAddr]
  }

makeLenses ''Announce

buildQuery :: Torrent -> Word32 -> Query
buildQuery torrent port =
  P.map
    (Just <$>)
    [ ("info_hash", torrent ^. infoHash)
    , ("peer_id", "asdfasdfasdfasdfasdf")
    , ("port", encode $ show port)
    , ("uploaded", encode $ show 0)
    , ("downloaded", encode $ show 0)
    , ("left", encode $ show (torrent ^. fileLength))
    ]
 where
  encode = encodeUtf8 . T.pack

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

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

createRequestAnnounce :: (MonadFail m) => Torrent -> URI -> Word32 -> m Request
createRequestAnnounce torrent trackerUri port = do
  request <-
    (parseRequest $ renderStr $ trackerUri)
      `orFail` "Can't reparse or announce is not present request"

  return $ addToRequestQueryString (buildQuery torrent port) request

parseResponse :: (MonadIO m, MonadFail m) => Bencode -> m Announce
parseResponse bencode = do
  interval <-
    (bencode ^? _BDict . ix "interval" . _BInteger)
      `orFail` "Can't extract interval from response"

  bencodedPeers <-
    (bencode ^? _BDict . ix "peers" . _BList)
      `orFail` "Response doesn't have peers"

  (Announce interval) . mconcat <$> mapM extractPeer bencodedPeers

getPeers :: (MonadIO m, MonadFail m) => Torrent -> URI -> Word32 -> m Announce
getPeers torrent trackerUri port = do
  request <- createRequestAnnounce torrent trackerUri port
  response <- httpBS request
  bencode <- parse $ responseBody response

  (bencode ^? _BDict . ix "failure")
    `thenFail` \err -> "Response contains failure " <> show err

  parseResponse bencode

createUdp = Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol

getPeersUdp :: (MonadIO m, MonadFail m) => Torrent -> URI -> Word32 -> m Announce
getPeersUdp torrent trackerUri port = do
  liftIO $ bracket createUdp Socket.close $ \socket -> do
    addr : _ <- Socket.getAddrInfo Nothing (Just "bt3.t-ru.org") (Just "http")
    Socket.connect socket (Socket.addrAddress addr)

    SocketBs.send socket $ B.pack $ [0, 0, 0x04, 0x17, 0x27, 0x10, 0x19, 0x80] <> [0, 0, 0, 0] <> [1, 2, 3, 4] <> [0, 0, 0, 0]
    print "sent, waiting for response"
    resp <- SocketBs.recv socket 16
    print resp

    fail "A"
