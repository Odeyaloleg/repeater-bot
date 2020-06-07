{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main where

import Control.Concurrent.Async (async)
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as BS8
import RepeaterBot.Config (HasConfig, getVal, parseConfig, readConfig)
import RepeaterBot.Settings (HasSettings, setBotSettings)
import Slack.Settings (SlackSettings)
import Slack.SlackBot (execSlackBot)
import System.IO (FilePath)
import Telegram.Settings (TelegramSettings)
import Telegram.TelegramBot (execTelegramBot)

main :: IO ()
main = do
  let configName = "bot.config" :: FilePath
  putStrLn $ "Reading config \"" ++ configName ++ "\"."
  config <- readConfig configName
  case config of
    Left e -> putStrLn $ "Config error: " ++ e
    Right settings -> do
      case getVal "Messenger" settings of
        Just "Telegram" -> do
          putStrLn "Starting Telegram Bot."
          let telegramSettings =
                setBotSettings settings :: Maybe TelegramSettings
          runBot execTelegramBot telegramSettings
        Just "Slack" -> do
          putStrLn "Starting Slack Bot."
          let slackSettings = setBotSettings settings :: Maybe SlackSettings
          runBot execSlackBot slackSettings
        Just _ -> putStrLn "Unknown messenger in config. Bot wasn't executed."
        Nothing ->
          putStrLn
            "Couldn't find field \"Messenger\" in config. Bot wasn't executed."

instance HasConfig FilePath where
  readConfig configName = do
    result <-
      try $ BS8.readFile configName :: IO (Either SomeException BS8.ByteString)
    case result of
      Left e -> return $ Left $ show e
      Right content -> return $ parseConfig content

runBot :: (HasSettings a) => (a -> IO ()) -> Maybe a -> IO ()
runBot bot settings =
  case settings of
    Nothing -> putStrLn "Couldn't parse settings properly. Bot wasn't executed."
    Just settings' -> bot settings'
