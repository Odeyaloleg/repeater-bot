{-# LANGUAGE OverloadedStrings #-}

module Telegram.Parsing where

import Data.Aeson

data TelegramUpdates = TelegramUpdates [TelegramMsg] | BadRequest String

instance FromJSON TelegramUpdates where
  parseJSON (Object updatesObject) = do
    isOk <- updatesObject .: "ok"
    case isOk of
      False -> BadRequest <$> updatesObject .: "description"
      True  -> TelegramUpdates <$> updatesObject .: "result"

data TelegramMsg = TelegramMsg
                     { updateId :: Int
                     , msgText  :: Maybe String
                     , sticker  :: Maybe TelegramSticker
                     , entities :: Maybe [TelegramEntity]
                     , chatId   :: Int }

instance FromJSON TelegramMsg where
  parseJSON (Object telegramMsg) = do
    msgObject  <- telegramMsg .:  "message"
    chatObject <- msgObject   .:  "chat"
    updateId   <- telegramMsg .:  "update_id"
    msgText    <- msgObject   .:? "text"
    sticker    <- msgObject   .:? "sticker"
    entities   <- msgObject   .:? "entities"
    chatId     <- chatObject  .:  "id"
    return $ TelegramMsg updateId msgText sticker entities chatId

data TelegramSticker = TelegramSticker
                         { stickerUniqueId :: String }

instance FromJSON TelegramSticker where
  parseJSON (Object stickerObject) = TelegramSticker <$> stickerObject .: "file_id"

data TelegramEntity = TelegramEntity
                        { offset       :: Int
                        , entityLength :: Int
                        , entityType   :: String 
                        , url          :: Maybe String }

instance FromJSON TelegramEntity where
  parseJSON (Object entityObject) = do
    offset     <- entityObject .:  "offset"
    length     <- entityObject .:  "length"
    entityType <- entityObject .:  "type"
    url        <- entityObject .:? "url"
    return $ TelegramEntity offset length entityType url