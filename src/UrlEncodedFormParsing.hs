{-# LANGUAGE OverloadedStrings #-}

module UrlEncodedFormParsing 
  ( parseData, getVal
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL8 ( ByteString, cons, foldr, head, tail
                                                    , singleton, append, take, drop, empty )
import qualified Data.Map.Strict as MS ( Map, fromList, lookup )
import           Data.Char ( chr )

parseData :: BSL8.ByteString -> MS.Map BSL8.ByteString BSL8.ByteString
parseData d =
  let dataLines = fst $ BSL8.foldr (\c (acc,dataLine) -> if c == '&' then (dataLine:acc,"") else (acc,c `BSL8.cons` dataLine)) ([],"") d
  in
  MS.fromList $ map inTuple dataLines
  where
  inTuple line = helper ("",line)
  helper (field,value) =
    let c = BSL8.head value
    in
    if c == '=' then (field,parseHexes $ BSL8.tail value) else helper (field `BSL8.append` BSL8.singleton c,BSL8.tail value)
  parseHexes v = BSL8.foldr (\c acc -> if c == '%' then BSL8.singleton (hexToChar $ BSL8.take 2 acc)`BSL8.append` BSL8.drop 2 acc else BSL8.singleton c `BSL8.append` acc) BSL8.empty v

getVal :: BSL8.ByteString -> MS.Map BSL8.ByteString BSL8.ByteString -> Maybe BSL8.ByteString
getVal field d = MS.lookup field d

hexToChar :: BSL8.ByteString -> Char
hexToChar hex = chr $ hexToInt hex
  where
  hexToInt h = fst $ BSL8.foldr (\c (acc,n) -> (acc + (fromHex c) * (16 ^ n),n+1)) (0,0) h
  fromHex x = case x of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    'A' -> 10
    'B' -> 11
    'C' -> 12
    'D' -> 13
    'E' -> 14
    'F' -> 15