{-# LANGUAGE OverloadedStrings #-}

module Logging
  ( LogLevel(..)
  , logTelegram
  , logSlack
  ) where

import qualified Data.ByteString.Lazy as BSL

data LogLevel
  = LevelDEBUG
  | LevelWARN
  | LevelRELEASE
  deriving (Eq, Ord)

logTelegram :: BSL.ByteString -> IO ()
logTelegram log = BSL.appendFile "Telegram.txt" (log `BSL.append` "\n")

logSlack :: BSL.ByteString -> IO ()
logSlack log = BSL.appendFile "Slack.txt" (log `BSL.append` "\n")
