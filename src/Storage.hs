module Storage (Storage) where

import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.Map as M

data Storage = Storage 
  { unStorage :: M.Map Int (M.Map Int B.ByteString)
  , hashes :: A.Array Int B.ByteString 
  } deriving Show

