{-# LANGUAGE OverloadedStrings #-}

module SlackBot
  ( execSlackBot
  ) where

import           Network.Wai hiding ( Response, requestBody, requestHeaders )
import           Network.HTTP.Types ( status200, hContentType )
import           Network.Wai.Handler.Warp ( runSettings, defaultSettings, setHost )
import           Network.HTTP.Simple ( httpJSON, httpLBS, parseRequest_, Response, getResponseBody )
import           Network.HTTP.Client.Internal ( method, requestBody, requestHeaders, RequestBody ( RequestBodyLBS ) )
import           Data.Aeson ( decode, encode )
import           Data.Aeson.Types ( Object )
import qualified Data.ByteString.Lazy as BSL ( fromStrict )
import qualified Data.ByteString.Lazy.Char8 as BSL8 ( putStrLn )
import qualified Data.ByteString.Char8 as BS8 ( pack )
import           Data.Streaming.Network.Internal ( HostPreference ( Host ) )
import           Slack.Parsing
import           Slack.ToJSON
import           Slack.Settings

execSlackBot :: SlackSettings -> IO ()
execSlackBot s@(SlackSettings botToken serverIP serverPort) = do
  runSettings (setHost (Host serverIP) defaultSettings) (application s)

application :: SlackSettings -> Application
application (SlackSettings botToken serverIP serverPort) request respond = do
  putStrLn $ "\nTriggered."
  reqBody <- strictRequestBody request
  let parsedResponse = decode reqBody :: Maybe SlackResponse
  case parsedResponse of
    Nothing -> respond $ responseLBS
                 status200
                 [(hContentType, "text/html")]
                 (BSL.fromStrict (BS8.pack "Couldn't handle data."))
    Just slackResponse -> case slackResponse of
      SlackTextMessage textMessage -> do
        case textMessage of
          Just textMessage' -> do
            let answerRequest = parseRequest_ "https://slack.com/api/chat.postMessage"
            answerResponse <- httpLBS $ answerRequest { method = "POST"
                                            , requestHeaders = [(hContentType, "application/json"), ("Authorization", BS8.pack ("Bearer " ++ botToken))]
                                            , requestBody = RequestBodyLBS $ encode $ SlackTextMessageJSON "" "CV2TXJJ59" textMessage' }
            BSL8.putStrLn $ getResponseBody answerResponse
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