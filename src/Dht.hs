{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Dht (find) where

import Bencode (
  Bencode (..),
  encode,
  parse,
  _BDict,
  _BList,
  _BString,
 )
import Utils (orFail, timeout, withUdp)
import Logger

import qualified Data.ByteString as B

import Control.Applicative (asum)
import Control.Exception (SomeException, try, catch, throw)
import Control.Lens
import Control.Monad
import Data.Map
import qualified Network.Socket as Socket
import Network.Socket.ByteString (recv, send)

import Prelude as P

data Node = Node {_nodeId :: B.ByteString, _address :: Socket.SockAddr} deriving (Eq, Show)

makeLenses ''Node

getPeers :: B.ByteString -> B.ByteString -> B.ByteString
getPeers id hash =
  Bencode.encode $
    BDict $
      fromList
        [ ("t", BString "00")
        , ("y", BString "q")
        , ("q", BString "get_peers")
        ,
          ( "a"
          , BDict $
              fromList
                [ ("id", BString id)
                , ("info_hash", BString hash)
                ]
          )
        ]

findNode :: B.ByteString -> B.ByteString -> B.ByteString
findNode id target =
  Bencode.encode $
    BDict $
      fromList
        [ ("t", BString "00")
        , ("y", BString "q")
        , ("q", BString "find_node")
        ,
          ( "a"
          , BDict $
              fromList
                [ ("id", BString id)
                , ("target", BString target)
                ]
          )
        ]

parseIpAndPort :: B.ByteString -> Socket.SockAddr
parseIpAndPort bstr = case P.splitAt 4 (B.unpack bstr) of
  ([o4, o3, o2, o1], [hi, lo]) ->
    let port = fromInteger $ toInteger hi * 256 + toInteger lo
     in Socket.SockAddrInet port $ Socket.tupleToHostAddress (o4, o3, o2, o1)
  _ -> error "Impossible"

parseNode :: B.ByteString -> Node
parseNode bstr =
  let (id, ipAndPort) = B.splitAt 20 bstr
      addr = parseIpAndPort ipAndPort
   in Node id addr

root :: Socket.SockAddr
root = Socket.SockAddrInet 25401 $ Socket.tupleToHostAddress (185, 157, 221, 247)

find :: B.ByteString -> IO [Socket.SockAddr]
find hash = find' [] root
 where
  find' :: [Socket.SockAddr] -> Socket.SockAddr -> IO [Socket.SockAddr]
  find' path current = withUdp $ \socket -> do
    when (current `P.elem` path) $ do
      putStrLnPar "Cycle. Exiting"
      fail "cycle"

    putStrLnPar $ show (P.length path) <> " " <> show current
    (Socket.connect socket current)
      `catch` (\(e :: SomeException) -> print e >> throw e)
    
    void $ send socket $ getPeers "asdfasdfasdfasdfasdf" hash

    let eitherToMaybe :: Either SomeException B.ByteString -> Maybe B.ByteString
        eitherToMaybe = either (const Nothing) Just

    maybeBencode <-
      try (timeout 1_000_000 $ recv socket 4096)
        <&> eitherToMaybe
        <&> (>>= Bencode.parse)

    bencode <- maybeBencode `orFail` "timeout"

    case (bencode ^? _BDict . (ix "r") . _BDict . (ix "values") . _BList) of
      Just values -> do
        putStrLnPar "finished"
        return $ P.map parseIpAndPort (values ^.. each . _BString)
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

          let newPath = current : path
          let addresses = P.map (find' newPath . view address) nodes

          asum $ addresses <> (if P.null path then [find' [] current] else [])