module Main where

import Config
import TelegramBot
import Telegram.Settings

main :: IO ()
main = do
  let configName = "bot.config"
  botConfig <- readConfig configName
  case botConfig of
    Nothing         -> return ()
    Just botConfig' -> do
      let telegramSettings = setTelegramSettings botConfig'
      case telegramSettings of
        Nothing               -> putStrLn "Couldn't parse Telegram settings properly. Telegram bot wasn't executed."
        Just telegramSettings' -> runTelegramBot 0 telegramSettings'