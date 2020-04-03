{-# LANGUAGE OverloadedStrings #-}

module Telegram.Parsing where

import Data.Aeson ( FromJSON ( parseJSON ), Value ( Object ), (.:), (.:?) )
import Data.Foldable ( asum )

data TelegramUpdates = TelegramUpdates [TelegramMsgUpdate] | BadRequest String

instance FromJSON TelegramUpdates where
  parseJSON (Object updatesObject) = do
    isOk <- updatesObject .: "ok"
    case isOk of
      False -> BadRequest <$> updatesObject .: "description"
      True  -> TelegramUpdates <$> updatesObject .: "result"

type UpdateId     = Int
type ChatId       = Int
type Offset       = Int
type EntityLength = Int
type EntityType   = String
type EntityUrl    = Maybe String

data TelegramMsgUpdate = TelegramMsgUpdate UpdateId ChatId TelegramMsg

instance FromJSON TelegramMsgUpdate where
  parseJSON (Object msgUpdateObject) = do
    TelegramMsgUpdate <$> msgUpdateObject .: "update_id"
                      <*> (msgUpdateObject .: "message"
                          >>= \messageObject -> messageObject .: "chat"
                          >>= \chatObject    -> chatObject .: "id")
                      <*> msgUpdateObject .: "message"

data TelegramMsg = TextMsg String (Maybe [TelegramEntity])
                  | StickerMsg String
                  | UnknownMsg

instance FromJSON TelegramMsg where
  parseJSON (Object msgObject) =
    asum [ TextMsg <$> msgObject .: "text" <*> msgObject .:? "entities"
         , StickerMsg <$> (msgObject .: "sticker"
                          >>= \stickerObject -> stickerObject .: "file_id")
         , return UnknownMsg ]

data TelegramEntity = TelegramEntity Offset EntityLength EntityType EntityUrl

instance FromJSON TelegramEntity where
  parseJSON (Object entityObject) =
    TelegramEntity <$> entityObject .:  "offset"
                   <*> entityObject .:  "length"
                   <*> entityObject .:  "type"
                   <*> entityObject .:? "url"