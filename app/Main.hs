module Main where

import Config
import Control.Concurrent.Async (async)
import Slack.Settings
import SlackBot
import Telegram.Settings
import TelegramBot

main :: IO ()
main = do
  let configName = "bot.config"
  botConfig <- readConfig configName
  case botConfig of
    Left e -> putStrLn $ "Config error: " ++ e
    Right botConfig' -> do
      let telegramSettings = setTelegramSettings botConfig'
      case telegramSettings of
        Nothing ->
          async $
          putStrLn $
          "Couldn't parse Telegram settings properly. Telegram bot wasn't executed."
        Just telegramSettings' -> async $ execTelegramBot telegramSettings'
      let slackSettings = setSlackSettings botConfig'
      case slackSettings of
        Nothing ->
          putStrLn $
          "Couldn't parse Slack settings properly. Slack bot wasn't executed."
        Just slackSettings' -> execSlackBot slackSettings'
