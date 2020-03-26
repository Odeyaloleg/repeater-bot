{-# LANGUAGE OverloadedStrings #-}

module SlackBot
  ( execSlackBot
  ) where

import qualified Network.Wai as WAI hiding ( requestBody, requestHeaders )
import           Network.HTTP.Types ( status200, hContentType )
import           Network.Wai.Handler.Warp ( runSettings, defaultSettings, setHost )
import           Network.HTTP.Simple ( httpJSON, httpLBS, parseRequest, Response, getResponseBody )
import           Network.HTTP.Client.Internal ( method, requestBody, requestHeaders, RequestBody ( RequestBodyLBS ) )
import           Data.Aeson ( ToJSON, decode, encode )
import           Data.Aeson.Types ( Object )
import qualified Data.ByteString.Lazy.Char8 as BSL8 ( putStrLn )
import qualified Data.ByteString.Char8 as BS8 ( pack )
import           Data.Streaming.Network.Internal ( HostPreference ( Host ) )
import           Slack.Parsing
import           Slack.ToJSON
import           Slack.Settings

type SlackMethod = String

botUri = "https://slack.com/api/" 

execSlackBot :: SlackSettings -> IO ()
execSlackBot (SlackSettings botToken (ServerSettings serverIP serverPort)) = do
  runSettings (setHost (Host serverIP) defaultSettings) (application botToken)

-- Use this to notify Slack about successful data receiving
dataRecieved = WAI.responseLBS status200 [] ""

application :: String -> WAI.Application
application botToken request respond = do
  putStrLn $ "\nTriggered."
  reqBody <- WAI.strictRequestBody request
  let parsedRequest = decode reqBody :: Maybe SlackMsg
  case parsedRequest of
    Nothing -> do
      respond $ dataRecieved
    Just slackMsg -> handleSlackMsg slackMsg botToken respond

handleSlackMsg :: SlackMsg -> String -> (WAI.Response -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
handleSlackMsg slackMsg botToken respond = case slackMsg of
  SlackTextMessage textMessage -> do
    case textMessage of
      Just textMessage' -> do
        sendAnswerNTimes 1 "chat.postMessage" botToken (SlackTextMessageJSON "CV2TXJJ59" textMessage')
        putStrLn "Text message was sent."
      Nothing -> putStrLn "Bot's msg."
    respond $ dataRecieved
  SlackChallenge challenge -> respond $ WAI.responseLBS
                                          status200
                                          [(hContentType, "application/json")]
                                          (encode (SlackChallengeJSON challenge))

sendAnswerNTimes :: (ToJSON a) => Int -> SlackMethod -> BotToken -> a -> IO (Response Object)
sendAnswerNTimes n slackMethod botToken slackMsg = do
  answer <- parseRequest $ botUri ++ slackMethod
  repeatAnswer n answer { method = "POST"
                        , requestHeaders = [(hContentType, "application/json"), ("Authorization", BS8.pack ("Bearer " ++ botToken))]
                        , requestBody = RequestBodyLBS $ encode slackMsg }
  where
  repeatAnswer 1 answer = httpJSON answer :: IO (Response Object)
  repeatAnswer n answer = do
    httpJSON answer :: IO (Response Object)
    repeatAnswer (n-1) answer