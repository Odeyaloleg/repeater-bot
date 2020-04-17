{-# LANGUAGE OverloadedStrings #-}

module Telegram.SendingMessages
  ( TelegramBotMsgJSON(..)
  , sendMessagesNTimes
  , botUri
  ) where

import Data.Aeson (ToJSON, encode)
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
import Telegram.Parsing (AnswerStatus(..))
import Telegram.Settings (RequestSettings(..))
import Telegram.ToJSON (BotStickerMsgJSON(..), BotTextMsgJSON(..))

type TelegramMethod = String

data TelegramBotMsgJSON
  = TextMsgJSON BotTextMsgJSON
  | StickerMsgJSON BotStickerMsgJSON

botUri = "https://api.telegram.org/bot"

sendMessagesNTimes ::
     [(TelegramBotMsgJSON, Int)] -> RequestSettings -> IO [AnswerStatus]
sendMessagesNTimes msgs settings = helper msgs settings []
  where
    helper [messageData] s answersStatus = do
      res <- sender messageData s
      return $ res ++ answersStatus
    helper (messageData:rest) s answersStatus = do
      res <- sender messageData s
      helper rest s (res ++ answersStatus)

sender :: (TelegramBotMsgJSON, Int) -> RequestSettings -> IO [AnswerStatus]
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
  -> IO [AnswerStatus]
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
    []
  where
    repeatAnswer 1 answer answersStatus = do
      response <- httpJSON answer
      let answerStatus = (getResponseBody response :: AnswerStatus)
      return $ answerStatus : answersStatus
    repeatAnswer n answer answersStatus = do
      response <- httpJSON answer
      let answerStatus = (getResponseBody response :: AnswerStatus)
      repeatAnswer (n - 1) answer (answerStatus : answersStatus)
