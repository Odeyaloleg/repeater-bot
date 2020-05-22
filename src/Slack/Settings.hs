{-# LANGUAGE OverloadedStrings #-}

module Slack.Settings
  ( SlackSettings(..)
  , SlackTextAnswers(..)
  , ServerSettings(..)
  , BotToken
  , RepetitionsNum
  ) where

import Logging (LogLevel)
import Settings (HasSettings, setBotSettings, getSettingString, getSettingInt, getRepetitions, getLogLevel)

type ServerIP = String

type ServerPort = Int

type BotToken = String

type RepetitionsNum = Int

data SlackSettings =
  SlackSettings BotToken ServerSettings RepetitionsNum SlackTextAnswers LogLevel
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
    parsedRepititions <- getRepetitions settingsMap
    parsedAboutMsg <- getSettingString "CommandHelp" settingsMap
    parsedRepeatMsg <- getSettingString "CommandRepeat" settingsMap
    parsedLogLevel <- getLogLevel "SlackLogLevel" settingsMap
    return $
      SlackSettings
        parsedToken
        (ServerSettings parsedServerIP parsedServerPort)
        parsedRepititions
        (SlackTextAnswers parsedAboutMsg parsedRepeatMsg)
        parsedLogLevel
