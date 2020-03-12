{-# LANGUAGE OverloadedStrings #-}

module ToJSON.Telegram where

import Data.Aeson

data TelegramMsgJSON = TelegramMsgJSON
                     { chatIdJSON  :: Integer
                     , parseType   :: Maybe String
                     , msgTextJSON :: String }

instance ToJSON TelegramMsgJSON where
  toJSON (TelegramMsgJSON chatId (Just parseType) msgText) =
    object [ "chat_id"    .= chatId
           , "parse_type" .= parseType
           , "text"       .= msgText ]
  toJSON (TelegramMsgJSON chatId Nothing msgText) =
    object [ "chat_id"   .= chatId
           , "text"      .= msgText ]

data TelegramStickerJSON = TelegramStickerJSON
                         { stickerChatId       :: Integer
                         , stickerUniqueIdJSON :: String }

instance ToJSON TelegramStickerJSON where
  toJSON (TelegramStickerJSON chatId stickerUniqueId) =
    object [ "chat_id" .= chatId
           , "sticker" .= stickerUniqueId ]