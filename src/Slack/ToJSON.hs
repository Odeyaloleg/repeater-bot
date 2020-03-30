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

data SlackCommandAnswerJSON = TextAnswerJSON String
                            | ButtonsAnswerJSON String

instance ToJSON SlackCommandAnswerJSON where
  toJSON (TextAnswerJSON textMessage) =
    object [ "text"          .= textMessage
           , "response_type" .= ("ephemeral" :: String) ]
  toJSON (ButtonsAnswerJSON textMessage) =
    object [ "response_type" .= ("ephemeral" :: String)
           , "blocks" .= [ object [ "type" .= ("section" :: String)
                                  , "text" .= object [ "type" .= ("mrkdwn" :: String)
                                                     , "text" .= textMessage ] ]
                         , object [ "type"     .= ("actions" :: String)
                                  , "elements" .= [ object [ "type" .= ("button" :: String)
                                                           , "text" .= object [ "type"  .= ("plain_text" :: String)
                                                                              , "text"  .= ("1" :: String)
                                                                              , "emoji" .= False ]
                                                           , "action_id" .= ("1" :: String) ]
                                                  , object [ "type" .= ("button" :: String)
                                                           , "text" .= object [ "type"  .= ("plain_text" :: String)
                                                                              , "text"  .= ("2" :: String)
                                                                              , "emoji" .= False ]
                                                           , "action_id" .= ("2" :: String) ]
                                                  , object [ "type" .= ("button" :: String)
                                                           , "text" .= object [ "type"  .= ("plain_text" :: String)
                                                                              , "text"  .= ("3" :: String)
                                                                              , "emoji" .= False ]
                                                           , "action_id" .= ("3" :: String) ]
                                                  , object [ "type" .= ("button" :: String)
                                                           , "text" .= object [ "type"  .= ("plain_text" :: String)
                                                                              , "text"  .= ("4" :: String)
                                                                              , "emoji" .= False ]
                                                           , "action_id" .= ("4" :: String) ]
                                                  , object [ "type" .= ("button" :: String)
                                                           , "text" .= object [ "type"  .= ("plain_text" :: String)
                                                                              , "text"  .= ("5" :: String)
                                                                              , "emoji" .= False ]
                                                           , "action_id" .= ("5" :: String) ] ] ] ] ]