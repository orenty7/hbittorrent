{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Torrent
  

main :: IO ()
main = do
  torrent <- parse <$> B.readFile "test4.torrent"
  print torrent
  
