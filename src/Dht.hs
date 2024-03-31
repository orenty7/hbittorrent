{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Dht where

import Bencode
import Control.Applicative (asum)
import Control.Concurrent
import Control.Exception (bracket, bracket_)
import Control.Lens
import Control.Monad
import Data.ByteString as B
import Data.ByteString.UTF8 as UTF8
import Data.Map
import Data.Word
import GHC.IO (unsafePerformIO)
import Network.Socket
import Network.Socket (SockAddr (SockAddrInet), tupleToHostAddress)
import Network.Socket.ByteString
import Parser (orFail)
import Torrent
import Prelude as P

type Peer = SockAddr

data Node = Node {_nodeId :: ByteString, _address :: SockAddr} deriving (Eq, Show)

makeLenses ''Node

getPeers :: ByteString -> ByteString -> ByteString
getPeers id hash =
  Bencode.encode $
    BDict $
      fromList
        [ ("t", BString "00"),
          ("y", BString "q"),
          ("q", BString "get_peers"),
          ( "a",
            BDict $
              fromList
                [ ("id", BString id),
                  ("info_hash", BString hash)
                ]
          )
        ]

findNode :: ByteString -> ByteString -> ByteString
findNode id target =
  Bencode.encode $
    BDict $
      fromList
        [ ("t", BString "00"),
          ("y", BString "q"),
          ("q", BString "find_node"),
          ( "a",
            BDict $
              fromList
                [ ("id", BString id),
                  ("target", BString target)
                ]
          )
        ]

createUdp = socket AF_INET Datagram defaultProtocol

withUdp action = bracket createUdp close action

-- root = "dht.libtorrent.org"

parseIpAndPort :: ByteString -> SockAddr
parseIpAndPort bstr =
  let ([o4, o3, o2, o1], [hi, lo]) = P.splitAt 4 (B.unpack bstr)
      port = fromInteger $ toInteger hi * 256 + toInteger lo
   in SockAddrInet port $ tupleToHostAddress (o4, o3, o2, o1)

parseNode :: ByteString -> Node
parseNode bstr =
  let (id, ipAndPort) = B.splitAt 20 bstr
      addr = parseIpAndPort ipAndPort
   in Node id addr

ping :: IO ()
ping = withUdp $ \socket -> do
  addr : _ <- getAddrInfo Nothing (Just "dht.libtorrent.org") (Just "25401")
  connect socket (addrAddress addr)
  bytesSend <- send socket "d1:ad2:id20:abcdefghij0123456789e1:q4:ping1:t2:aa1:y1:qe"
  (Just bencode) <- Bencode.parse <$> recv socket 4096
  print bencode

root :: SockAddr
root = SockAddrInet 25401 $ tupleToHostAddress (185, 157, 221, 247)

lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

putStrLnPar :: String -> IO ()
putStrLnPar x = do
  takeMVar lock
  putStrLn x
  putMVar lock ()

printPar :: (Show a) => a -> IO ()
printPar = putStrLnPar . show

timeout :: Int -> IO a -> IO (Maybe a)
timeout mcs action = do
  var <- newEmptyMVar

  producer <- forkIO $ do
    res <- action
    putMVar var $ Just res

  killer <- forkIO $ do
    threadDelay mcs
    killThread producer
    putMVar var Nothing

  res <- takeMVar var
  mapM_ killThread [producer, killer]
  return res

find :: Int -> SockAddr -> IO [ByteString]
find depth current = withUdp $ \socket -> do
  -- let hash = B.pack [251, 14, 56, 46, 94, 129, 26, 83, 25, 203, 24, 157, 210, 41, 101, 14, 223, 56, 93, 54]
  let hash = B.pack $ P.reverse [248, 54, 50, 120, 33, 15, 248, 0, 115, 158, 39, 251, 208, 0, 69, 30, 193, 205, 128, 0]
  -- addr : _ <- getAddrInfo Nothing (Just "dht.libtorrent.org") (Just "25401")
  putStrLnPar $ show depth <> " " <> show current
  connect socket current
  send socket $ getPeers "asdfasdfasdfasdfasdf" hash
  maybeBencode <- (>>= Bencode.parse) <$> timeout 1000000 (recv socket 4096)
  bencode <- maybeBencode `orFail` "timeout"

  case (bencode ^? _BDict . (ix "r") . _BDict . (ix "values") . _BList) of
    Just values -> do
      putStrLnPar "finished"
      return (values ^.. each . _BString)
    _ -> case (bencode ^? _BDict . (ix "r") . _BDict . (ix "nodes") . _BString) of
      Nothing -> fail "Error"
      Just packedNodes -> do
        let nodes = P.map parseNode $ go packedNodes
              where
                go packed
                  | B.length packed == 0 = []
                  | B.length packed >= 26 = node : go rest
                  | otherwise = error "Incorrect length"
                  where
                    (node, rest) = B.splitAt 26 packed
        -- printPar nodes
        let addresses = P.map (Dht.find (depth + 1) . view address) nodes

        asum $ addresses <> [Dht.find depth current]
