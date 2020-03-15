{-# LANGUAGE OverloadedStrings #-}

module Telegram.ToJSON where

import Data.Aeson

data TelegramMsgJSON = TelegramMsgJSON
                     { chatIdJSON  :: Int
                     , parseMode   :: Maybe String
                     , msgTextJSON :: String
                     , replyMarkup :: Maybe TelegramReplyMarkup }

instance ToJSON TelegramMsgJSON where
  toJSON (TelegramMsgJSON chatId (Just parseMode) msgText rMarkup) =
    object [ "chat_id"    .= chatId
           , "parse_mode" .= parseMode
           , "text"       .= msgText ]
  toJSON (TelegramMsgJSON chatId Nothing msgText (Just rMarkup)) =
    object [ "chat_id"      .= chatId
           , "text"         .= msgText
           , "reply_markup" .= rMarkup ]
  toJSON (TelegramMsgJSON chatId Nothing msgText Nothing) =
    object [ "chat_id"      .= chatId
           , "text"         .= msgText ]

data TelegramStickerJSON = TelegramStickerJSON
                         { stickerChatId       :: Int
                         , stickerUniqueIdJSON :: String }

instance ToJSON TelegramStickerJSON where
  toJSON (TelegramStickerJSON chatId stickerUniqueId) =
    object [ "chat_id" .= chatId
           , "sticker" .= stickerUniqueId ]

data TelegramKBButton = TelegramKBButton
                      { buttonText :: String }

instance ToJSON TelegramKBButton where
  toJSON (TelegramKBButton btnText) =
    object [ "text" .= btnText ]

data TelegramReplyMarkup = TelegramKBMarkup [[TelegramKBButton]]
                         | TelegramKBRemove

instance ToJSON TelegramReplyMarkup where
  toJSON (TelegramKBMarkup kb) =
    object [ "keyboard"        .= kb ]
  toJSON TelegramKBRemove =
    object [ "remove_keyboard" .= True ]