{-# LANGUAGE OverloadedStrings #-}

module SlackBot
  ( execSlackBot
  ) where

import           Network.Wai hiding ( Response, requestBody, requestHeaders )
import           Network.HTTP.Types ( status200, hContentType )
import           Network.Wai.Handler.Warp ( runSettings, defaultSettings, setHost )
import           Network.HTTP.Simple ( httpJSON, httpLBS, parseRequest_, Response, getResponseBody )
import           Network.HTTP.Client.Internal ( method, requestBody, requestHeaders, RequestBody ( RequestBodyLBS ) )
import           Data.Aeson ( ToJSON, decode, encode )
import           Data.Aeson.Types ( Object )
import qualified Data.ByteString.Lazy as BSL ( fromStrict )
import qualified Data.ByteString.Lazy.Char8 as BSL8 ( putStrLn )
import qualified Data.ByteString.Char8 as BS8 ( pack )
import           Data.Streaming.Network.Internal ( HostPreference ( Host ) )
import           Slack.Parsing
import           Slack.ToJSON
import           Slack.Settings

type SlackMethod = String

execSlackBot :: SlackSettings -> IO ()
execSlackBot (SlackSettings botToken (ServerSettings serverIP serverPort)) = do
  runSettings (setHost (Host serverIP) defaultSettings) (application botToken)

-- Use this to notify Slack about successful data receiving
dataRecieved = responseLBS status200 [] ""

application :: String -> Application
application botToken request respond = do
  putStrLn $ "\nTriggered."
  reqBody <- strictRequestBody request
  let parsedRequest = decode reqBody :: Maybe SlackMsg
  case parsedRequest of
    Nothing -> do
      respond $ dataRecieved
    Just slackMsg -> case slackMsg of
      SlackTextMessage textMessage -> do
        case textMessage of
          Just textMessage' -> do
            let answerRequest = parseRequest_ "https://slack.com/api/chat.postMessage"
            httpJSON $ answerRequest { method = "POST"
                                     , requestHeaders = [(hContentType, "application/json"), ("Authorization", BS8.pack ("Bearer " ++ botToken))]
                                     , requestBody = RequestBodyLBS $ encode $ SlackTextMessageJSON "" "CV2TXJJ59" textMessage' } :: IO (Response Object)
            putStrLn "Text message was sent."
          Nothing -> do
            putStrLn "Bot's msg."
        respond $ dataRecieved
      SlackChallenge challenge -> respond $ responseLBS
                                              status200
                                              [(hContentType, "application/json")]
                                              (encode (SlackChallengeJSON challenge))

sendAnswerNTimes :: (ToJSON a) => Int -> SlackMethod -> BotToken -> a -> IO (Response Object)
sendAnswerNTimes = undefined