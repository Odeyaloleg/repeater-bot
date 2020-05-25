{-# LANGUAGE OverloadedStrings #-}

module Slack.Settings
  ( SlackSettings(..)
  , SlackTextAnswers(..)
  , ServerSettings(..)
  , BotToken
  , RepetitionsNum
  , SlackEnv(..)
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Logger
  ( LogLevel(..)
  , Logger
  , logDebug
  , logRelease
  , logWarning
  , writeLogIn
  )
import Settings
  ( HasSettings
  , getLogLevel
  , getRepetitions
  , getSettingInt
  , getSettingString
  , setBotSettings
  )

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

data SlackEnv =
  SlackEnv
    { botToken :: BotToken
    , textAnswers :: SlackTextAnswers
    , repetitionsNum :: RepetitionsNum
    , logLevel :: LogLevel
    }

instance Logger SlackEnv where
  logDebug log = do
    logLvl <- asks logLevel
    when (logLvl == LevelDEBUG) $ liftIO $ writeLogIn "Slack.txt" log
  logWarning log = do
    logLvl <- asks logLevel
    when (logLvl <= LevelWARN) $ liftIO $ writeLogIn "Slack.txt" log
  logRelease log = do
    logLvl <- asks logLevel
    when (logLvl <= LevelRELEASE) $ liftIO $ writeLogIn "Slack.txt" log
