{-# LANGUAGE OverloadedStrings #-}

module SlackBot
  ( execSlackBot
  ) where

import qualified Network.Wai as WAI hiding ( requestBody )
import           Network.HTTP.Types ( status200, hContentType )
import           Network.Wai.Handler.Warp ( runSettings, defaultSettings, setHost )
import           Network.HTTP.Simple ( httpJSON, httpLBS, parseRequest, Response, getResponseBody )
import           Network.HTTP.Client.Internal ( method, requestBody, requestHeaders, RequestBody ( RequestBodyLBS ) )
import           Data.Aeson ( ToJSON, decode, encode )
import           Data.Aeson.Types ( Object )
import qualified Data.ByteString.Lazy.Char8 as BSL8 ( ByteString, putStrLn, toStrict, unpack )
import qualified Data.ByteString.Char8 as BS8 ( pack, breakSubstring, dropWhile, putStrLn )
import           Data.Streaming.Network.Internal ( HostPreference ( Host ) )
import           Data.Maybe ( fromJust )
import           UrlEncodedFormParsing
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
  let reqHeaders = WAI.requestHeaders request
  reqBody <- WAI.strictRequestBody request
  case lookup hContentType reqHeaders of
    Just "application/json"                  -> do
      let parsedRequest = decode reqBody :: Maybe SlackMsg
      case parsedRequest of
        Nothing -> do
          putStrLn "Unknown JSON data."
          respond $ dataRecieved
        Just slackMsg -> handleSlackMsg slackMsg botToken respond
    Just "application/x-www-form-urlencoded" -> do
      let requestData = parseData reqBody
      case getVal "command" requestData of
        Nothing -> do
          putStrLn "Unknown urlencoded data."
          respond $ dataRecieved
        Just command -> do
          let channelId = fromJust $ getVal "channel_id" requestData
          handleSlashCommand command channelId botToken respond
      respond $ dataRecieved
    Nothing -> do
      putStrLn "Unknown content."
      respond $ dataRecieved

handleSlackMsg :: SlackMsg -> String -> (WAI.Response -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
handleSlackMsg slackMsg botToken respond = case slackMsg of
  SlackTextMessage channelId textMessage -> do
    sendAnswerNTimes 1 "chat.postMessage" botToken (SlackTextMessageJSON channelId textMessage)
    putStrLn "Text message was sent."
    respond $ dataRecieved
  SlackOwnMessage -> do
    putStrLn "Own message."
    respond $ dataRecieved
  SlackChallenge challenge -> respond $ WAI.responseLBS
                                          status200
                                          [(hContentType, "application/json")]
                                          (encode (SlackChallengeJSON challenge))

handleSlashCommand :: BSL8.ByteString -> BSL8.ByteString -> String -> (WAI.Response -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
handleSlashCommand command channelId botToken respond = do
  case command of
    "/about"  -> sendAnswerNTimes 1 "chat.postMessage" botToken (SlackTextMessageJSON (BSL8.unpack channelId) "Info")
    "/repeat" -> sendAnswerNTimes 1 "chat.postMessage" botToken (SlackTextMessageJSON (BSL8.unpack channelId) "Repeating")
    _         -> sendAnswerNTimes 1 "chat.postMessage" botToken (SlackTextMessageJSON (BSL8.unpack channelId) "There is no handler for this command.")
  respond $ dataRecieved

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