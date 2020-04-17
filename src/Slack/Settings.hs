{-# LANGUAGE OverloadedStrings #-}

module Slack.Settings
  ( SlackSettings(..)
  , SlackTextAnswers(..)
  , ServerSettings(..)
  , BotToken
  , setSlackSettings
  ) where

import qualified Data.ByteString.Char8 as BS8
  ( ByteString
  , null
  , readInt
  , unpack
  )
import qualified Data.Map.Strict as MS (Map, lookup)

type ServerIP = String

type ServerPort = Int

type BotToken = String

data SlackSettings =
  SlackSettings BotToken ServerSettings SlackTextAnswers

data ServerSettings =
  ServerSettings ServerIP ServerPort

data SlackTextAnswers =
  SlackTextAnswers
    { aboutText :: String
    , repeatText :: String
    }

setSlackSettings :: MS.Map BS8.ByteString BS8.ByteString -> Maybe SlackSettings
setSlackSettings settingsMap = do
  parsedToken <-
    MS.lookup "SlackToken" settingsMap >>=
    (\token ->
       if BS8.null token
         then Nothing
         else Just (BS8.unpack token))
  parsedServerIP <-
    MS.lookup "ServerIP" settingsMap >>=
    (\ip ->
       if BS8.null ip
         then Nothing
         else Just (BS8.unpack ip))
  parsedServerPort <-
    MS.lookup "ServerPort" settingsMap >>=
    (\port ->
       if BS8.null port
         then Nothing
         else case BS8.readInt port of
                Nothing -> Nothing
                Just (port', _) -> Just port')
  parsedAboutMsg <-
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
    SlackSettings
      parsedToken
      (ServerSettings parsedServerIP parsedServerPort)
      (SlackTextAnswers parsedAboutMsg parsedRepeatMsg)
