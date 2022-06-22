{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Bencode where

import qualified Data.Text as T
import Data.Char (isDigit)
import qualified Data.Map as M


type Dict = M.Map T.Text Bencode

type ErrorMessage = String
type Result = Either ErrorMessage

data Bencode = L [Bencode] | I Integer | D Dict | S T.Text
  deriving (Eq, Show)


encode :: Bencode -> T.Text
encode (I int) = "i" <> (T.pack . show) int <> "e"
encode (S str) = (T.pack . show . T.length) str <> ":" <> str
encode (L list) = "l" <> mconcat (map encode list) <> "e"
encode (D dict) = "d" <> mconcat (map
                                   (\(key, value) ->
                                       encode (S key) <> encode value)
                                   (M.toList dict)) <> "e"
                  


decode :: T.Text -> Result (Bencode, T.Text)
decode (T.uncons -> Nothing)  = Left "Empty String"
decode raw@(T.uncons -> Just (x, xs)) 
  | x == 'i' = 
    if xs == "" then
      Left "Unclosed Int"
    else
      let (int, rest) = T.break (== 'e') xs in
        if T.null rest then
          Left "Unclosed Int"
        else if T.any (not . isDigit) (T.tail int) ||
                T.head int /= '-' && not (isDigit (T.head int))
             then             
          Left "Error, only digits and heading '-' allowed in int"
        else
          return (I $ read $ T.unpack int, T.tail rest)

  | x == 'l' = let
      extract :: T.Text -> Result ([Bencode], T.Text)
      extract (T.uncons -> Nothing)  = Left "List is not closed"
      extract (T.uncons -> Just ('e', xs)) = return ([], xs)
      extract xs = do
        (value, rest') <- decode xs
        (list, rest) <- extract rest'
        return (value:list, rest)
    in    
      case extract xs of
        Right (list, rest) -> return (L list, rest)
        Left err_msg -> Left err_msg


  | x == 'd' = let
      extract :: T.Text -> Result ([(T.Text, Bencode)], T.Text)
      extract (T.uncons -> Nothing) = Left "Dict is not closed"
      extract (T.uncons -> Just ('e', xs)) = return ([], xs)
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
      (len', rest') = T.break (== ':') raw
      len = read (T.unpack len')
      rest = T.tail rest'
    in      
      if T.any (not . isDigit) len' then
        Left "Incorrect string length or missing ':'"
      else if T.null rest' || T.length rest < len then
        Left "Incorrect, string are too short "
      else
        return $ (S $ T.take len rest, T.drop len rest)
     
decode _ = Left "Incorrect bencode"      
    
  
  



