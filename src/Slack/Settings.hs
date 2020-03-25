{-# LANGUAGE OverloadedStrings #-}

module Slack.Settings
  where

import qualified Data.Map.Strict as MS        ( Map, lookup )
import qualified Data.ByteString.Char8 as BS8 ( ByteString, null, unpack, readInt )
import           Network.HTTP.Client          ( Proxy ( Proxy ) )
import           Control.Monad                ( (>>=) )

type ServerIP   = String
type ServerPort = Int
type BotToken   = String

data SlackSettings = SlackSettings BotToken ServerIP ServerPort

setSlackSettings :: MS.Map BS8.ByteString BS8.ByteString -> Maybe SlackSettings
setSlackSettings settingsMap = do
  parsedToken       <- MS.lookup "SlackToken" settingsMap >>= (\token -> if BS8.null token then Nothing else Just (BS8.unpack token))
  parsedServerIP    <- MS.lookup "ServerIP" settingsMap >>= (\ip -> if BS8.null ip then Nothing else Just (BS8.unpack ip))
  parsedServerPort <- MS.lookup "ServerPort" settingsMap >>=
    (\port -> if BS8.null port then Nothing else
      case BS8.readInt port of
        Nothing -> Nothing
        Just (port', _) -> Just port')
  return $ SlackSettings parsedToken parsedServerIP parsedServerPort
