{-# LANGUAGE OverloadedStrings #-}

module SlackBot
  ( execSlackBot
  ) where

import           Network.Wai hiding ( Response, requestBody, requestHeaders )
import           Network.HTTP.Types ( status200, hContentType )
import           Network.Wai.Handler.Warp ( runSettings, defaultSettings, setHost)
import           Network.HTTP.Simple ( httpJSON, httpLBS, parseRequest_, Response, getResponseBody )
import           Network.HTTP.Client.Internal ( method, requestBody, requestHeaders, RequestBody ( RequestBodyLBS ) )
import           Data.Aeson ( decode, encode )
import           Data.Aeson.Types ( Object )
import qualified Data.ByteString.Lazy as BSL ( fromStrict )
import qualified Data.ByteString.Lazy.Char8 as BSL8 ( putStrLn )
import qualified Data.ByteString.Char8 as BS8 ( pack )
import           Slack.Parsing
import           Slack.ToJSON

execSlackBot :: IO ()
execSlackBot = do
  runSettings (setHost "192.168.0.139" defaultSettings) application

slackToken = BS8.pack "Bearer <token>"

application :: Application
application request respond = do
  putStrLn "\nTriggered."
  requestBody1 <- strictRequestBody request
  let parsedResponse = decode requestBody1 :: Maybe SlackResponse
  case parsedResponse of
    Nothing -> respond $ responseLBS
                 status200
                 [(hContentType, "text/html")]
                 (BSL.fromStrict (BS8.pack "Couldn't handle data."))
    Just slackResponse -> case slackResponse of
      SlackTextMessage textMessage -> do
        case textMessage of
          Just textMessage' -> do
            let request1 = parseRequest_ "https://slack.com/api/chat.postMessage"
            response1 <- httpLBS $ request1 { method = "POST"
                                            , requestBody = RequestBodyLBS $ encode $ SlackTextMessageJSON "" "CV2TXJJ59" textMessage'
                                            , requestHeaders = [(hContentType, "application/json"), ("Authorization", slackToken)] }
            BSL8.putStrLn $ getResponseBody response1
            putStrLn "Text message was sent."
          Nothing -> return ()
        respond $ responseLBS
                  status200
                  [(hContentType, "application/json")]
                  (BSL.fromStrict (BS8.pack "Success."))
      SlackChallenge challenge -> respond $ responseLBS
                                            status200
                                            [(hContentType, "application/json")]
                                            (encode (SlackChallengeJSON challenge))