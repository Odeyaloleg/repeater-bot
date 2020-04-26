{-# LANGUAGE OverloadedStrings #-}

module Telegram.Settings
  ( TelegramSettings(..)
  , RequestSettings(..)
  , setTelegramSettings
  ) where

import qualified Data.ByteString.Char8 as BS8
  ( ByteString
  , null
  , readInt
  , unpack
  )
import qualified Data.Map.Strict as MS (Map, lookup)
import Network.HTTP.Client (Proxy(Proxy))

data TelegramSettings =
  TelegramSettings
    { requestSettings :: RequestSettings
    , pollingTimeout :: Int
    , repeatsNum :: Int
    , helpMessage :: String
    , repeatMessage :: String
    }
  deriving (Eq)

data RequestSettings =
  RequestSettings
    { botToken :: String
    , proxyServer :: Maybe Proxy
    }
  deriving (Eq)

class SettingsTelegram a where
  setTelegramSettings :: MS.Map a a -> Maybe TelegramSettings

instance SettingsTelegram BS8.ByteString where
  setTelegramSettings settingsMap = do
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
    return $
      TelegramSettings
        (RequestSettings parsedToken parsedProxyServer)
        parsedPollingTimeout
        parsedRepititions
        parsedHelpMsg
        parsedRepeatMsg
