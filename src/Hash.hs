module Hash (Hash, mkHash, unHash, hash, check) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B

newtype Hash = Hash { unHash :: B.ByteString } deriving (Eq, Show)

mkHash :: B.ByteString -> Hash
mkHash bstr 
  | B.length bstr == 20 = Hash bstr
  | otherwise = error "incorrect hash length"

hash :: B.ByteString -> Hash
hash bstr = Hash $ SHA1.hash bstr

check :: B.ByteString -> Hash -> Bool
check bstr (Hash hash) = SHA1.hash bstr == hash 


