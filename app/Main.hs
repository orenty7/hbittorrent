{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Torrent
import Filesystem  

main :: IO ()
main = do
  (Right torrent) <- parse <$> B.readFile "test.torrent"
  print torrent
  markup torrent

  
  
