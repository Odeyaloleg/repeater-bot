{-# LANGUAGE OverloadedStrings #-}

module Telegram.Settings
  ( TelegramSettings(..)
  , RequestSettings(..)
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Char8 as BS8
import Logger
  ( LogLevel(..)
  , Logger
  , logDebug
  , logRelease
  , logWarning
  , writeLogIn
  )
import Network.HTTP.Client (Proxy(Proxy))
import Settings
  ( HasSettings
  , getLogLevel
  , getRepetitions
  , getSettingInt
  , getSettingString
  , setBotSettings
  )

data TelegramSettings =
  TelegramSettings
    { requestSettings :: RequestSettings
    , pollingTimeout :: Int
    , repetitionsNum :: Int
    , helpMessage :: String
    , repeatMessage :: String
    , incorrectAnswer :: String
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
    parsedToken <- getSettingString "TelegramToken" settingsMap
    parsedProxyServer <-
      getSettingString "ProxyIP" settingsMap >>=
      (\ip ->
         getSettingInt "ProxyPort" settingsMap >>=
         (\port -> Just $ Just $ Proxy (BS8.pack ip) port))
    parsedPollingTimeout <- getSettingInt "PollingTimeout" settingsMap
    parsedRepititions <- getRepetitions settingsMap
    parsedHelpMsg <- getSettingString "CommandHelp" settingsMap
    parsedRepeatMsg <- getSettingString "CommandRepeat" settingsMap
    parsedIncorrectAnswer <- getSettingString "IncorrectAnswer" settingsMap
    parsedLogLevel <- getLogLevel "TelegramLogLevel" settingsMap
    return $
      TelegramSettings
        (RequestSettings parsedToken parsedProxyServer)
        parsedPollingTimeout
        parsedRepititions
        parsedHelpMsg
        parsedRepeatMsg
        parsedIncorrectAnswer
        parsedLogLevel

instance Logger TelegramSettings where
  logDebug log = do
    logLvl <- asks logLevel
    when (logLvl == LevelDEBUG) $ liftIO $ writeLogIn "Telegram.txt" log
  logWarning log = do
    logLvl <- asks logLevel
    when (logLvl <= LevelWARN) $ liftIO $ writeLogIn "Telegram.txt" log
  logRelease log = do
    logLvl <- asks logLevel
    when (logLvl <= LevelRELEASE) $ liftIO $ writeLogIn "Telegram.txt" log
