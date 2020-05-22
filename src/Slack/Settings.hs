{-# LANGUAGE OverloadedStrings #-}

module Slack.Settings
  ( SlackSettings(..)
  , SlackTextAnswers(..)
  , ServerSettings(..)
  , BotToken
  ) where

import Logging (LogLevel)
import Settings (HasSettings, setBotSettings, getSettingString, getSettingInt, getLogLevel)

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
    parsedToken <- getSettingString "SlackToken" settingsMap
    parsedServerIP <- getSettingString "ServerIP" settingsMap
    parsedServerPort <- getSettingInt "ServerPort" settingsMap
    parsedAboutMsg <- getSettingString "CommandHelp" settingsMap
    parsedRepeatMsg <- getSettingString "CommandRepeat" settingsMap
    parsedLogLevel <- getLogLevel "SlackLogLevel" settingsMap
    return $
      SlackSettings
        parsedToken
        (ServerSettings parsedServerIP parsedServerPort)
        (SlackTextAnswers parsedAboutMsg parsedRepeatMsg)
        parsedLogLevel
