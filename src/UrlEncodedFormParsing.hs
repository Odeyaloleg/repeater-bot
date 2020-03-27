{-# LANGUAGE OverloadedStrings #-}

module UrlEncodedFormParsing 
  ( parseData, getVal
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL8 ( ByteString, cons, foldr, head, tail
                                                    , singleton, append )
import qualified Data.Map.Strict as MS ( Map, fromList, lookup )

parseData :: BSL8.ByteString -> MS.Map BSL8.ByteString BSL8.ByteString
parseData d =
  let dataLines = fst $ BSL8.foldr (\c (acc,dataUnit) -> if c == '&' then (dataUnit:acc,"") else (acc,c `BSL8.cons` dataUnit)) ([],"") d
  in
  MS.fromList $ map inTuple dataLines
  where
  inTuple line = helper ("",line) 
  helper (field,value) =
    let c = BSL8.head value
    in
    if c == '=' then (field,BSL8.tail value) else helper (field `BSL8.append` BSL8.singleton c,BSL8.tail value)

-- Name WIP
getVal :: BSL8.ByteString -> MS.Map BSL8.ByteString BSL8.ByteString -> Maybe BSL8.ByteString
getVal field d = MS.lookup field d