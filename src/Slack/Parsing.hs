{-# LANGUAGE OverloadedStrings #-}

module Slack.Parsing where

import Data.Aeson
import Data.Aeson.Types ( Parser )

-- Bad architecture. Will refactor after more deep understanding of Slack.

type ChannelId = String

data SlackMsg = SlackChallenge String
              | SlackTextMessage ChannelId String
              | SlackOwnMessage

instance FromJSON SlackMsg where
  parseJSON (Object response) = do
    responseType <- response .: "type" :: Parser String
    case responseType of
      "url_verification" -> do
        challenge <- response .: "challenge"
        return $ SlackChallenge challenge
      "event_callback" -> do
        eventObj <- response .: "event"
        botId <- eventObj .:? "bot_id" :: Parser (Maybe String)
        case botId of
          Nothing -> do
            textMessage <- eventObj .: "text"
            channel     <- eventObj .: "channel"
            return $ SlackTextMessage channel textMessage
          _ -> return $ SlackOwnMessage