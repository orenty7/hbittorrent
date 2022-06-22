{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Bencode (encode, decode, Dict, Bencode (..) ) where


import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import qualified Data.Map as M


type Dict = M.Map B.ByteString Bencode

type ErrorMessage = String
type Result = Either ErrorMessage

data Bencode = L [Bencode] | I Integer | D Dict | S B.ByteString
  deriving (Eq, Show)


encode :: Bencode -> B.ByteString
encode (I int) = "i" <> (B.pack . show) int <> "e"
encode (S str) = (B.pack . show . B.length) str <> ":" <> str
encode (L list) = "l" <> mconcat (map encode list) <> "e"
encode (D dict) = "d" <> mconcat (map
                                   (\(key, value) ->
                                       encode (S key) <> encode value)
                                   (M.toList dict)) <> "e"
                  


decode :: B.ByteString -> Result (Bencode, B.ByteString)
decode (B.uncons -> Nothing)  = Left "Empty String"
decode raw@(B.uncons -> Just (x, xs)) 
  | x == 'i' = 
    if xs == "" then
      Left "Unclosed Int"
    else
      let (int, rest) = B.break (== 'e') xs in
        if B.null rest then
          Left "Unclosed Int"
        else if B.any (not . isDigit) (B.tail int) ||
                B.head int /= '-' && not (isDigit (B.head int))
             then             
          Left "Error, only digits and heading '-' allowed in int"
        else
          return (I $ read $ B.unpack int, B.tail rest)

  | x == 'l' = let
      extract :: B.ByteString -> Result ([Bencode], B.ByteString)
      extract (B.uncons -> Nothing)  = Left "List is not closed"
      extract (B.uncons -> Just ('e', xs)) = return ([], xs)
      extract xs = do
        (value, rest') <- decode xs
        (list, rest) <- extract rest'
        return (value:list, rest)
    in    
      case extract xs of
        Right (list, rest) -> return (L list, rest)
        Left err_msg -> Left err_msg


  | x == 'd' = let
      extract :: B.ByteString -> Result ([(B.ByteString, Bencode)], B.ByteString)
      extract (B.uncons -> Nothing) = Left "Dict is not closed"
      extract (B.uncons -> Just ('e', xs)) = return ([], xs)
      extract xs = do
        (key, rest'') <- decode xs
        (value, rest') <- decode rest''
        (pairs, rest) <- extract rest'

        case key of
          S str -> return $ ((str, value):pairs, rest)
          _ -> Left "Error, expected key (string)"     
      in
        case extract xs of
          Right (pairs, rest) -> return (D $ M.fromList pairs, rest)
          Left err_msg -> Left err_msg  
  

  | isDigit x = let
      (len', rest') = B.break (== ':') raw
      len = read (B.unpack len')
      rest = B.tail rest'
    in      
      if B.any (not . isDigit) len' then
        Left "Incorrect string length or missing ':'"
      else if B.null rest' || B.length rest < len then
        Left "Incorrect, string are too short "
      else
        return $ (S $ B.take len rest, B.drop len rest)
     
decode _ = Left "Incorrect bencode"      
    
  
  



