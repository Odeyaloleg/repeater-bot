{-# LANGUAGE OverloadedStrings #-}

module Slack.SendingMessages
  ( sendAnswerNTimes
  , sendAnswerToCommand
  ) where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Network.HTTP.Client.Internal as HTTP.Internal
import Network.HTTP.Simple
  ( getResponseBody
  , getResponseStatusCode
  , httpJSON
  , httpNoBody
  , parseRequest
  )
import Network.HTTP.Types (hContentType)
import RepeaterBot.Logger (logDebug, logRelease)
import Slack.Parsing (AnswerStatus(..))
import Slack.Settings (SlackEnv(..))

type SlackMethod = String

botUri = "https://slack.com/api/"

sendAnswerNTimes ::
     (ToJSON a) => Int -> SlackMethod -> a -> ReaderT SlackEnv IO [AnswerStatus]
sendAnswerNTimes n slackMethod slackMsg = do
  token <- asks botToken
  answer <- lift $ parseRequest $ botUri ++ slackMethod
  logDebug "Sending answer to Slack."
  repeatAnswer
    n
    answer
      { HTTP.Internal.method = "POST"
      , HTTP.Internal.requestHeaders =
          [ (hContentType, "application/json")
          , ("Authorization", BS8.pack ("Bearer " ++ token))
          ]
      , HTTP.Internal.requestBody =
          HTTP.Internal.RequestBodyLBS $ encode slackMsg
      }
    []
  where
    repeatAnswer 1 answer answersStatus = do
      response <- httpJSON answer
      let answerStatus = getResponseBody response :: AnswerStatus
      return $ answerStatus : answersStatus
    repeatAnswer n answer answersStatus = do
      response <- httpJSON answer
      let answerStatus = getResponseBody response :: AnswerStatus
      repeatAnswer (n - 1) answer (answerStatus : answersStatus)

sendAnswerToCommand ::
     (ToJSON a) => String -> a -> ReaderT SlackEnv IO AnswerStatus
sendAnswerToCommand responseUrl slackAnswerToCommand = do
  slackAnswer <-
    lift $
    parseRequest responseUrl >>=
    (\request ->
       httpNoBody
         request
           { HTTP.Internal.method = "POST"
           , HTTP.Internal.requestHeaders = [(hContentType, "application/json")]
           , HTTP.Internal.requestBody =
               HTTP.Internal.RequestBodyLBS $ encode slackAnswerToCommand
           })
  case getResponseStatusCode slackAnswer of
    200 -> do
      logDebug "Successfully sent answer onto command."
      return AnswerSuccess
    code -> do
      logRelease $
        "Couldn't send answer onto command, status code: " `BSL.append`
        BSL8.pack (show code)
      return AnswerFail
