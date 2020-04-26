{-# LANGUAGE OverloadedStrings #-}

module Slack.Parsing
  ( SlackMsg(..)
  , SlackPayload(..)
  ) where

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Aeson.Types (Parser)

type ChannelId = String

type UserId = String

data SlackMsg
  = SlackChallenge String
  | SlackTextMessage ChannelId UserId String
  | SlackOwnMessage
  deriving (Eq)

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
            channel <- eventObj .: "channel"
            user <- eventObj .: "user"
            textMessage <- eventObj .: "text"
            return $ SlackTextMessage channel user textMessage
          _ -> return $ SlackOwnMessage

data SlackPayload
  = SlackPayloadButton UserId String
  | UnknownPayload

instance FromJSON SlackPayload where
  parseJSON (Object response) = do
    actions <- response .: "actions"
    if null actions
      then return UnknownPayload
      else do
        userId <- response .: "user" >>= (\userObject -> userObject .: "id")
        actionId <- head actions .: "action_id"
        return $ SlackPayloadButton userId actionId
