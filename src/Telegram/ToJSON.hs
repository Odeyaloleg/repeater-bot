{-# LANGUAGE OverloadedStrings #-}

module Telegram.ToJSON
  ( BotTextMsgJSON(..)
  , BotStickerMsgJSON(..)
  , TelegramKBButton(..)
  , TelegramReplyMarkup(..)
  ) where

import Data.Aeson (ToJSON(toJSON), (.=), object)

type ChatId = Int

type ParseMode = Maybe String

type TextMessage = String

type ReplyMarkup = Maybe TelegramReplyMarkup

type StickerId = String

data BotTextMsgJSON =
  BotTextMsgJSON ChatId ParseMode TextMessage ReplyMarkup

instance ToJSON BotTextMsgJSON where
  toJSON (BotTextMsgJSON chatId (Just parseMode) msgText rMarkup) =
    object ["chat_id" .= chatId, "parse_mode" .= parseMode, "text" .= msgText]
  toJSON (BotTextMsgJSON chatId Nothing msgText (Just rMarkup)) =
    object ["chat_id" .= chatId, "text" .= msgText, "reply_markup" .= rMarkup]
  toJSON (BotTextMsgJSON chatId Nothing msgText Nothing) =
    object ["chat_id" .= chatId, "text" .= msgText]

data BotStickerMsgJSON =
  BotStickerMsgJSON ChatId StickerId

instance ToJSON BotStickerMsgJSON where
  toJSON (BotStickerMsgJSON chatId stickerUniqueId) =
    object ["chat_id" .= chatId, "sticker" .= stickerUniqueId]

newtype TelegramKBButton =
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
