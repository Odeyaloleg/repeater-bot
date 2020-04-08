{-# LANGUAGE OverloadedStrings #-}

module Telegram.SendingMessages where

import Data.Aeson (ToJSON, encode)
import Data.Aeson.Types (Object)
import Network.HTTP.Client.Internal
  ( RequestBody(RequestBodyLBS)
  , Response
  , ResponseTimeout(ResponseTimeoutNone)
  , method
  , parseRequest
  , proxy
  , requestBody
  , requestHeaders
  )
import Network.HTTP.Simple (getResponseBody, httpJSON)
import qualified Network.HTTP.Types as HTTP (hContentType)
import Telegram.Settings
import Telegram.ToJSON
import UsersData

type TelegramMethod = String

data TelegramBotMsgJSON
  = TextMsgJSON BotTextMsgJSON
  | StickerMsgJSON BotStickerMsgJSON

botUri = "https://api.telegram.org/bot"

sendMessagesNTimes ::
     [(TelegramBotMsgJSON, Int)] -> RequestSettings -> IO (Response Object)
sendMessagesNTimes [messageData] s = sender messageData s
sendMessagesNTimes (messageData:rest) s = do
  sender messageData s
  sendMessagesNTimes rest s

sender :: (TelegramBotMsgJSON, Int) -> RequestSettings -> IO (Response Object)
sender messageData s = do
  let (botMsg, repetitionsNum) = messageData
  case botMsg of
    TextMsgJSON botMsg -> sendAnswerNTimes repetitionsNum "sendMessage" s botMsg
    StickerMsgJSON botMsg ->
      sendAnswerNTimes repetitionsNum "sendSticker" s botMsg

sendAnswerNTimes ::
     (ToJSON a)
  => Int
  -> TelegramMethod
  -> RequestSettings
  -> a
  -> IO (Response Object)
sendAnswerNTimes n telegramMethod s botMessage = do
  answer <- parseRequest $ botUri ++ botToken s ++ "/" ++ telegramMethod
  repeatAnswer
    n
    answer
      { method = "POST"
      , proxy = proxyServer s
      , requestHeaders = [(HTTP.hContentType, "application/json")]
      , requestBody = RequestBodyLBS $ encode $ botMessage
      }
  where
    repeatAnswer 1 answer = httpJSON answer :: IO (Response Object)
    repeatAnswer n answer = do
      httpJSON answer :: IO (Response Object)
      repeatAnswer (n - 1) answer