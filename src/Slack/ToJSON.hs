{-# LANGUAGE OverloadedStrings #-}

module Slack.ToJSON where

import Data.Aeson

data SlackChallengeJSON = SlackChallengeJSON String

instance ToJSON SlackChallengeJSON where
  toJSON (SlackChallengeJSON challenge) =
    object [ "challenge" .= challenge ]

data SlackTextMessageJSON = SlackTextMessageJSON String String

instance ToJSON SlackTextMessageJSON where
  toJSON (SlackTextMessageJSON channel textMessage) =
    object [ "channel" .= channel
           , "text"    .= textMessage ]