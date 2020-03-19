module Main where

import Control.Concurrent.Async ( async )
import Config
import TelegramBot
import Telegram.Settings
import SlackBot

main :: IO ()
main = do
  let configName = "bot.config"
  botConfig <- readConfig configName
  case botConfig of
    Left e           -> putStrLn $ "Config error: " ++ e
    Right botConfig' -> do

      let telegramSettings = setTelegramSettings botConfig'
      case telegramSettings of
        Nothing               -> async $ putStrLn "Couldn't parse Telegram settings properly. Telegram bot wasn't executed." -- I don't like this being in separate thread. But how else?
        Just telegramSettings' -> async $ execTelegramBot telegramSettings'

      execSlackBot