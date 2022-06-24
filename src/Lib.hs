{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Network.Socket as N
import Data.ByteString as B


data Prefix = B | K | M | G | T

toPrefix :: Integer -> Prefix
toPrefix size | 0      <= size && size <= 1024   = B
              | 1024   <= size && size <= 1024^2 = K
              | 1024^2 <= size && size <= 1024^3 = M
              | 1024^3 <= size && size <= 1024^4 = G
              | 1024^4 <= size && size <= 1024^5 = T



