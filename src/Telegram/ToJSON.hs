{-# LANGUAGE OverloadedStrings #-}

module Telegram.ToJSON
  ( TelegramBotMsgJSON(..)
  , TelegramKBButton(..)
  , TelegramReplyMarkup(..)
  ) where

import Data.Aeson (ToJSON(toJSON), (.=), object)

type ChatId = Int

type ParseMode = Maybe String

type TextMessage = String

type ReplyMarkup = Maybe TelegramReplyMarkup

type StickerId = String

data TelegramBotMsgJSON
  = BotTextMsgJSON ChatId ParseMode TextMessage ReplyMarkup
  | BotStickerMsgJSON ChatId StickerId

instance ToJSON TelegramBotMsgJSON where
  toJSON (BotTextMsgJSON chatId (Just parseMode) msgText rMarkup) =
    object ["chat_id" .= chatId, "parse_mode" .= parseMode, "text" .= msgText]
  toJSON (BotTextMsgJSON chatId Nothing msgText (Just rMarkup)) =
    object ["chat_id" .= chatId, "text" .= msgText, "reply_markup" .= rMarkup]
  toJSON (BotTextMsgJSON chatId Nothing msgText Nothing) =
    object ["chat_id" .= chatId, "text" .= msgText]
  toJSON (BotStickerMsgJSON chatId stickerUniqueId) =
    object ["chat_id" .= chatId, "sticker" .= stickerUniqueId]

data TelegramKBButton =
  TelegramKBButton
    { buttonText :: String
    }

instance ToJSON TelegramKBButton where
  toJSON (TelegramKBButton btnText) = object ["text" .= btnText]

data TelegramReplyMarkup
  = TelegramKBMarkup [[TelegramKBButton]]
  | TelegramKBRemove

instance ToJSON TelegramReplyMarkup where
  toJSON (TelegramKBMarkup kb) = object ["keyboard" .= kb]
  toJSON TelegramKBRemove = object ["remove_keyboard" .= True]
