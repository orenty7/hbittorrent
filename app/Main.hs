{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.Map (toList)
import Torrent
  

main :: IO ()
main = do
  torrent <- parse <$> B.readFile "test1.torrent"
  print torrent
  
