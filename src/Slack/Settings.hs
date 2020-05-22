{-# LANGUAGE OverloadedStrings #-}

module Slack.Settings
  ( SlackSettings(..)
  , SlackTextAnswers(..)
  , ServerSettings(..)
  , BotToken
  ) where

import qualified Data.ByteString.Char8 as BS8
  ( ByteString
  , null
  , readInt
  , unpack
  )
import Data.Char (toUpper)
import qualified Data.Map.Strict as MS (Map, lookup)
import Logging (LogLevel(..))
import Settings (HasSettings, setBotSettings)

type ServerIP = String

type ServerPort = Int

type BotToken = String

data SlackSettings =
  SlackSettings BotToken ServerSettings SlackTextAnswers LogLevel
  deriving (Eq)

data ServerSettings =
  ServerSettings ServerIP ServerPort
  deriving (Eq)

data SlackTextAnswers =
  SlackTextAnswers
    { aboutText :: String
    , repeatText :: String
    }
  deriving (Eq)

instance HasSettings SlackSettings where
  setBotSettings settingsMap = do
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
    parsedLogLevel <-
      MS.lookup "SlackLogLevel" settingsMap >>=
      (\logLevel ->
         if BS8.null logLevel
           then Nothing
           else case map toUpper (BS8.unpack logLevel) of
                  "DEBUG" -> Just LevelDEBUG
                  "WARN" -> Just LevelWARN
                  "RELEASE" -> Just LevelRELEASE)
    return $
      SlackSettings
        parsedToken
        (ServerSettings parsedServerIP parsedServerPort)
        (SlackTextAnswers parsedAboutMsg parsedRepeatMsg)
        parsedLogLevel
