{-# LANGUAGE OverloadedStrings #-}

module Parsing.Telegram where

import Data.Aeson

data TelegramMsgs = TelegramMsgs [TelegramMsg] | BadRequest { description :: String }

instance FromJSON TelegramMsgs where
  parseJSON (Object telegramMsgs) = do
    isOk <- telegramMsgs .: "ok"
    case isOk of
      False -> do
        description <- telegramMsgs .: "description"
        return $ BadRequest description
      True  -> do
        msgs <- telegramMsgs .: "result"
        return $ TelegramMsgs msgs

data TelegramMsg = TelegramMsg
                     { updateId :: Integer
                     , msgText  :: Maybe String
                     , sticker  :: Maybe TelegramSticker
                     , entities :: Maybe [TelegramEntity]
                     , chatId   :: Integer }

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
  parseJSON (Object stickerObject) = do
    stickerId <- stickerObject .: "file_id"
    return $ TelegramSticker stickerId

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