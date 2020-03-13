{-# LANGUAGE OverloadedStrings #-}

module Telegram.Settings where

import qualified Data.Map.Strict as MS        ( Map, lookup )
import qualified Data.ByteString.Char8 as BS8 ( ByteString, null, unpack, readInt )
import           Data.Maybe                   ( fromJust )
import           Network.HTTP.Client          ( Proxy  ( Proxy ) )
import           Control.Monad                ( (>>=) )

data TelegramSettings = TelegramSettings
                          { botToken       :: String
                          , proxyServer    :: Maybe Proxy
                          , pollingTimeout :: Int }

setTelegramSettings :: MS.Map BS8.ByteString BS8.ByteString -> Maybe TelegramSettings
setTelegramSettings settingsMap = do
  parsedToken          <- MS.lookup "TelegramToken" settingsMap >>= (\token -> if BS8.null token then Nothing else Just (BS8.unpack token))
  parsedProxyServer    <- MS.lookup "ProxyIP" settingsMap >>=
    (\ip -> if BS8.null ip then Just Nothing else MS.lookup "ProxyPort" settingsMap >>=
      (\port -> if BS8.null port then Nothing else do
        case BS8.readInt port of
          Nothing -> Nothing
          Just (port', _) -> Just (Just (Proxy ip port'))))
  parsedPollingTimeout <-
    MS.lookup "PollingTimeout" settingsMap >>=
    (\n -> if BS8.null n then Nothing else do
      case BS8.readInt n of
        Nothing -> Nothing
        Just (n', _) -> Just n')
  return $ TelegramSettings parsedToken parsedProxyServer parsedPollingTimeout
