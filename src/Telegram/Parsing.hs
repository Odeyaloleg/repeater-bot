{-# LANGUAGE OverloadedStrings #-}

module Telegram.Parsing
  ( TelegramUpdates(..)
  , TelegramMsgUpdate(..)
  , TelegramEntity(..)
  , TelegramMsg(..)
  , ChatId
  , AnswerStatus(..)
  ) where

import Data.Aeson (FromJSON(parseJSON), Object, Value(Object), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Foldable (asum)
import Telegram.BotModel (ChatId)

data TelegramUpdates
  = TelegramUpdates [TelegramMsgUpdate]
  | BadRequest String

instance FromJSON TelegramUpdates where
  parseJSON (Object updatesObject) = do
    isOk <- updatesObject .: "ok"
    if isOk
      then TelegramUpdates <$> updatesObject .: "result"
      else BadRequest <$> updatesObject .: "description"

type UpdateId = Int

type Offset = Int

type EntityLength = Int

type EntityType = String

type EntityUrl = Maybe String

data TelegramMsgUpdate =
  TelegramMsgUpdate UpdateId ChatId TelegramMsg

instance FromJSON TelegramMsgUpdate where
  parseJSON (Object msgUpdateObject) =
    TelegramMsgUpdate <$> msgUpdateObject .: "update_id" <*>
    (msgUpdateObject .: "message" >>= \messageObject ->
       messageObject .: "chat" >>= \chatObject -> chatObject .: "id") <*>
    msgUpdateObject .: "message"

data TelegramMsg
  = TextMsg String (Maybe [TelegramEntity])
  | StickerMsg String
  | UnknownMsg

instance FromJSON TelegramMsg where
  parseJSON (Object msgObject) =
    asum
      [ TextMsg <$> msgObject .: "text" <*> msgObject .:? "entities"
      , StickerMsg <$>
        (msgObject .: "sticker" >>= \stickerObject -> stickerObject .: "file_id")
      , return UnknownMsg
      ]

data TelegramEntity =
  TelegramEntity Offset EntityLength EntityType EntityUrl

instance FromJSON TelegramEntity where
  parseJSON (Object entityObject) =
    TelegramEntity <$> entityObject .: "offset" <*> entityObject .: "length" <*>
    entityObject .: "type" <*>
    entityObject .:? "url"

data AnswerStatus
  = AnswerSuccess
  | AnswerFail
  deriving (Eq)

instance FromJSON AnswerStatus where
  parseJSON (Object answerObject) = do
    isOk <- answerObject .: "ok"
    if isOk
      then return AnswerSuccess
      else return AnswerFail
