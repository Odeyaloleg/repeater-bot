{-# LANGUAGE OverloadedStrings #-}

module Settings where

import qualified Data.ByteString.Char8 as BS8
  ( ByteString
  , null
  , readInt
  , unpack
  )
import qualified Data.Map.Strict as MS
import Logging (LogLevel(..))
import Data.Char (toUpper)

class HasSettings a where
  setBotSettings :: MS.Map BS8.ByteString BS8.ByteString -> Maybe a

getSettingString ::
     BS8.ByteString -> MS.Map BS8.ByteString BS8.ByteString -> Maybe String
getSettingString setting settingsMap =
  MS.lookup setting settingsMap >>=
  (\s ->
     if BS8.null s
       then Nothing
       else Just (BS8.unpack s))

getSettingInt ::
     BS8.ByteString -> MS.Map BS8.ByteString BS8.ByteString -> Maybe Int
getSettingInt setting settingsMap =
  MS.lookup setting settingsMap >>=
  (\i ->
     if BS8.null i
       then Nothing
       else case BS8.readInt i of
              Just (i', _) -> Just i'
              _ -> Nothing)

getRepetitions :: MS.Map BS8.ByteString BS8.ByteString -> Maybe Int
getRepetitions settingsMap =
  MS.lookup "RepetitionsNumber" settingsMap >>=
  (\n ->
     if BS8.null n
       then Nothing
       else do
         case BS8.readInt n of
           Just (n', _) ->
             if n' > 0 && n' < 6
               then Just n'
               else Nothing
           _ -> Nothing)

getLogLevel ::
     BS8.ByteString -> MS.Map BS8.ByteString BS8.ByteString -> Maybe LogLevel
getLogLevel messengerLogLevel settingsMap =
  MS.lookup messengerLogLevel settingsMap >>=
  (\logLevel ->
          if BS8.null logLevel
            then Nothing
            else case map toUpper (BS8.unpack logLevel) of
                    "DEBUG" -> Just LevelDEBUG
                    "WARN" -> Just LevelWARN
                    "RELEASE" -> Just LevelRELEASE)