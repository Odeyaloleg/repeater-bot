{-# LANGUAGE OverloadedStrings #-}

module Slack.Parsing where

import Data.Aeson
import Data.Aeson.Types ( Parser )

-- Bad architecture. Will refactor after more deep understanding of Slack.

data SlackResponse = SlackChallenge String | SlackTextMessage (Maybe String)

instance FromJSON SlackResponse where
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
            return $ SlackTextMessage (Just textMessage)
          _ -> return $ SlackTextMessage Nothing