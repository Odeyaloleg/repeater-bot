{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (readConfig, getVal)
import Settings (HasSettings, setBotSettings)
import Control.Concurrent.Async (async)
import Slack.Settings (SlackSettings)
import SlackBot (execSlackBot)
import Telegram.Settings (TelegramSettings)
import TelegramBot (execTelegramBot)

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
          let telegramSettings = setBotSettings settings :: Maybe TelegramSettings
          runBot execTelegramBot telegramSettings
        Just "Slack" -> do
          putStrLn "Starting Slack Bot."
          let slackSettings = setBotSettings settings :: Maybe SlackSettings
          runBot execSlackBot slackSettings
        Just _ -> putStrLn "Unknown messenger in config. Bot wasn't executed."
        Nothing -> putStrLn "Couldn't find field \"Messenger\" in config. Bot wasn't executed."

runBot :: (HasSettings a) => (a -> IO ()) -> Maybe a -> IO ()
runBot bot settings =
  case settings of
    Nothing -> putStrLn "Couldn't parse settings properly. Bot wasn't executed."
    Just settings' -> bot settings'
