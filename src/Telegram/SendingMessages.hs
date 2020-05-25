{-# LANGUAGE OverloadedStrings #-}

module Telegram.SendingMessages
  ( TelegramBotMsgJSON(..)
  , sendMessagesNTimes
  ) where

import Control.Monad.Reader (ReaderT, asks)
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
import Telegram.BotModel (botUri)
import Telegram.Parsing (AnswerStatus(..))
import Telegram.Settings (RequestSettings(..), TelegramSettings(..))
import Telegram.ToJSON (BotStickerMsgJSON(..), BotTextMsgJSON(..))

type TelegramMethod = String

data TelegramBotMsgJSON
  = TextMsgJSON BotTextMsgJSON
  | StickerMsgJSON BotStickerMsgJSON

sendMessagesNTimes ::
     [(TelegramBotMsgJSON, Int)] -> ReaderT TelegramSettings IO [AnswerStatus]
sendMessagesNTimes msgs = helper msgs []
  where
    helper [messageData] answersStatus = do
      res <- sender messageData
      return $ res ++ answersStatus
    helper (messageData:rest) answersStatus = do
      res <- sender messageData
      helper rest (res ++ answersStatus)

sender ::
     (TelegramBotMsgJSON, Int) -> ReaderT TelegramSettings IO [AnswerStatus]
sender messageData = do
  let (botMsg, repetitionsNum) = messageData
  case botMsg of
    TextMsgJSON botMsg -> sendAnswerNTimes repetitionsNum "sendMessage" botMsg
    StickerMsgJSON botMsg ->
      sendAnswerNTimes repetitionsNum "sendSticker" botMsg

sendAnswerNTimes ::
     (ToJSON a)
  => Int
  -> TelegramMethod
  -> a
  -> ReaderT TelegramSettings IO [AnswerStatus]
sendAnswerNTimes n telegramMethod botMessage = do
  settings <- asks requestSettings
  answer <- parseRequest $ botUri ++ botToken settings ++ "/" ++ telegramMethod
  repeatAnswer
    n
    answer
      { method = "POST"
      , proxy = proxyServer settings
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
