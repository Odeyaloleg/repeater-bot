module Main where

import Config (readConfig)
import Control.Concurrent.Async (async)
import Slack.Settings (setSlackSettings)
import SlackBot (execSlackBot)
import Telegram.Settings (setTelegramSettings)
import TelegramBot (execTelegramBot)

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
          putStrLn $
          "Couldn't parse Telegram settings properly. Telegram bot wasn't executed."
        Just telegramSettings' -> do
          async $ execTelegramBot telegramSettings'
          return ()
      let slackSettings = setSlackSettings botConfig'
      case slackSettings of
        Nothing ->
          putStrLn $
          "Couldn't parse Slack settings properly. Slack bot wasn't executed."
        Just slackSettings' -> execSlackBot slackSettings'
