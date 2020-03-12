{-# LANGUAGE OverloadedStrings #-}

module Telegram.ToJSON where

import Data.Aeson

data TelegramMsgJSON = TelegramMsgJSON
                     { chatIdJSON  :: Int
                     , parseMode   :: Maybe String
                     , msgTextJSON :: String }

instance ToJSON TelegramMsgJSON where
  toJSON (TelegramMsgJSON chatId (Just parseMode) msgText) =
    object [ "chat_id"    .= chatId
           , "parse_mode" .= parseMode
           , "text"       .= msgText ]
  toJSON (TelegramMsgJSON chatId Nothing msgText) =
    object [ "chat_id"   .= chatId
           , "text"      .= msgText ]

data TelegramStickerJSON = TelegramStickerJSON
                         { stickerChatId       :: Int
                         , stickerUniqueIdJSON :: String }

instance ToJSON TelegramStickerJSON where
  toJSON (TelegramStickerJSON chatId stickerUniqueId) =
    object [ "chat_id" .= chatId
           , "sticker" .= stickerUniqueId ]