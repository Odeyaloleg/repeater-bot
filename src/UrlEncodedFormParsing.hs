{-# LANGUAGE OverloadedStrings #-}

module UrlEncodedFormParsing
  ( parseUrlEncoded
  , getVal
  , hexesToChars
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Char (chr)
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as MS (Map, fromList, lookup)
import Data.Maybe (fromJust)

parseUrlEncoded :: BSL8.ByteString -> MS.Map BSL8.ByteString String
parseUrlEncoded d =
  let dataLines =
        fst $
        BSL8.foldr
          (\c (acc, dataLine) ->
             if c == '&'
               then if BSL8.null dataLine
                      then (acc, "")
                      else (dataLine : acc, "")
               else (acc, c `BSL8.cons` dataLine))
          ([], "")
          d
   in MS.fromList $ map inTuple dataLines
  where
    inTuple line = helper ("", line)
    helper (field, value) =
      let c = BSL8.head value
       in if c == '='
            then (field, BSL8.unpack $ hexesToChars $ BSL8.tail value)
            else helper (field `BSL8.append` BSL8.singleton c, BSL8.tail value)

hexesToChars :: BSL8.ByteString -> BSL8.ByteString
hexesToChars v =
  BSL8.foldr
    (\c acc ->
       if c == '%'
         then maybe
                (BSL8.singleton c `BSL8.append` acc)
                (\x -> BSL8.singleton x `BSL8.append` BSL8.drop 2 acc)
                (hexToChar $ BSL8.take 2 acc)
         else BSL8.singleton c `BSL8.append` acc)
    ""
    v

getVal :: BSL8.ByteString -> MS.Map BSL8.ByteString String -> Maybe String
getVal field d = MS.lookup field d

hexToChar :: BSL8.ByteString -> Maybe Char
hexToChar hex = maybe Nothing (Just . chr) (toDecimal $ BSL8.unpack hex)
  where
    toDecimal h =
      maybe
        Nothing
        (Just . fst)
        (foldrM
           (\c (acc, n) ->
              maybe
                Nothing
                (\x -> Just (acc + x * (16 ^ n), n + 1))
                (hexCharToNum c))
           (0, 0)
           h)
    hexCharToNum x =
      case x of
        '0' -> Just 0
        '1' -> Just 1
        '2' -> Just 2
        '3' -> Just 3
        '4' -> Just 4
        '5' -> Just 5
        '6' -> Just 6
        '7' -> Just 7
        '8' -> Just 8
        '9' -> Just 9
        'A' -> Just 10
        'B' -> Just 11
        'C' -> Just 12
        'D' -> Just 13
        'E' -> Just 14
        'F' -> Just 15
        _ -> Nothing
