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

type Port = Word32

data Announce = Announce
  { _interval :: Integer,
    _peers :: [(HostName, Port)]
  }

makeLenses ''Announce

buildQuery :: Torrent -> Query
buildQuery torrent =
  P.map
    (Just <$>)
    [ ("info_hash", torrent ^. infoHash),
      ("peer_id", "asdfasdfasdfasdfasdf"),
      ("port", encode $ show 6881),
      ("uploaded", encode $ show 0),
      ("downloaded", encode $ show 0),
      ("left", encode $ show (torrent ^. fileLength))
    ]
  where
    encode = encodeUtf8 . T.pack

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

extractPeer :: (MonadFail m) => Bencode -> m (HostName, Port)
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

  return (T.unpack $ decodeUtf8 host, fromInteger port)

createRequestAnnounce :: (MonadFail m) => Torrent -> m Request
createRequestAnnounce torrent = do
  request <-
    parseRequest (renderStr (view announce torrent))
      `orFail` "Can't reparse request"

  return $ addToRequestQueryString (buildQuery torrent) request

parseResponse :: (MonadFail m) => Bencode -> m Announce
parseResponse bencode = do
  interval <-
    (bencode ^? _BDict . ix "interval" . _BInteger)
      `orFail` "Can't extract interval from response"

  bencodedPeers <-
    (bencode ^? _BDict . ix "peers" . _BList)
      `orFail` "Response doesn't have peers"

  return $ Announce interval (mapMaybe extractPeer bencodedPeers)

getPeers :: (MonadIO m, MonadFail m) => Torrent -> m Announce
getPeers torrent = do
  request <- createRequestAnnounce torrent
  response <- httpBS request
  bencode <- parse $ responseBody response

  (bencode ^? _BDict . ix "failure")
    `thenFail` \err -> "Response contains failure " <> show err

  parseResponse bencode