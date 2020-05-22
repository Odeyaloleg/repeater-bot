{-# LANGUAGE OverloadedStrings #-}

module Telegram.Settings
  ( TelegramSettings(..)
  , RequestSettings(..)
  ) where

import qualified Data.ByteString.Char8 as BS8
  ( ByteString
  , null
  , readInt
  , unpack
  )
import Data.Char (toUpper)
import qualified Data.Map.Strict as MS (lookup)
import Logging (LogLevel(..))
import Network.HTTP.Client (Proxy(Proxy))
import Settings (HasSettings, setBotSettings)

data TelegramSettings =
  TelegramSettings
    { requestSettings :: RequestSettings
    , pollingTimeout :: Int
    , repeatsNum :: Int
    , helpMessage :: String
    , repeatMessage :: String
    , logLevel :: LogLevel
    }
  deriving (Eq)

data RequestSettings =
  RequestSettings
    { botToken :: String
    , proxyServer :: Maybe Proxy
    }
  deriving (Eq)

instance HasSettings TelegramSettings where
  setBotSettings settingsMap = do
    parsedToken <-
      MS.lookup "TelegramToken" settingsMap >>=
      (\token ->
         if BS8.null token
           then Nothing
           else Just (BS8.unpack token))
    parsedProxyServer <-
      MS.lookup "ProxyIP" settingsMap >>=
      (\ip ->
         if BS8.null ip
           then Just Nothing
           else MS.lookup "ProxyPort" settingsMap >>=
                (\port ->
                   if BS8.null port
                     then Nothing
                     else do
                       case BS8.readInt port of
                         Nothing -> Nothing
                         Just (port', _) -> Just (Just (Proxy ip port'))))
    parsedPollingTimeout <-
      MS.lookup "PollingTimeout" settingsMap >>=
      (\n ->
         if BS8.null n
           then Nothing
           else do
             case BS8.readInt n of
               Nothing -> Nothing
               Just (n', _) -> Just n')
    parsedRepititions <-
      MS.lookup "RepetitionsNumber" settingsMap >>=
      (\n ->
         if BS8.null n
           then Nothing
           else do
             case BS8.readInt n of
               Nothing -> Nothing
               Just (n', _) ->
                 if n' > 0 && n' < 6
                   then Just n'
                   else Nothing)
    parsedHelpMsg <-
      MS.lookup "CommandHelp" settingsMap >>=
      (\t ->
         if BS8.null t
           then Nothing
           else Just (BS8.unpack t))
    parsedRepeatMsg <-
      MS.lookup "CommandRepeat" settingsMap >>=
      (\t ->
         if BS8.null t
           then Nothing
           else Just (BS8.unpack t))
    parsedLogLevel <-
      MS.lookup "TelegramLogLevel" settingsMap >>=
      (\logLevel ->
         if BS8.null logLevel
           then Nothing
           else case map toUpper (BS8.unpack logLevel) of
                  "DEBUG" -> Just LevelDEBUG
                  "WARN" -> Just LevelWARN
                  "RELEASE" -> Just LevelRELEASE)
    return $
      TelegramSettings
        (RequestSettings parsedToken parsedProxyServer)
        parsedPollingTimeout
        parsedRepititions
        parsedHelpMsg
        parsedRepeatMsg
        parsedLogLevel
