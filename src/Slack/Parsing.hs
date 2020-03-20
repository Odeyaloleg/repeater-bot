{-# LANGUAGE OverloadedStrings #-}

module Slack.Parsing where

import Data.Aeson
import Data.Aeson.Types ( Parser )

-- Bad architecture. Will refactor after more deep understanding of Slack.

data SlackResponse = SlackResponse
                       { slackChallenge   :: Maybe String
                       , slackTextMessage :: Maybe String }

instance FromJSON SlackResponse where
  parseJSON (Object response) = do
    responseType <- response .: "type" :: Parser String
    case responseType of
      "url_verification" -> do
        challenge <- response .: "challenge"
        return $ SlackResponse challenge Nothing
      "event_callback" -> do
        eventObj <- response .: "event"
        botId <- eventObj .:? "bot_id" :: Parser (Maybe String)
        case botId of
          Nothing -> do
            textMessage <- eventObj .: "text"
            return $ SlackResponse Nothing (Just textMessage)
          _ -> return $ SlackResponse Nothing Nothing