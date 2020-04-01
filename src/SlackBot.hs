{-# LANGUAGE OverloadedStrings #-}

module SlackBot
  ( execSlackBot
  ) where

import qualified Network.Wai as WAI hiding ( requestBody )
import           Network.HTTP.Types ( status200, hContentType )
import           Network.Wai.Handler.Warp ( runSettings, defaultSettings, setHost )
import           Network.HTTP.Simple ( httpJSON, httpNoBody, parseRequest, Response, getResponseBody )
import           Network.HTTP.Client.Internal ( method, requestBody, requestHeaders, RequestBody ( RequestBodyLBS ) )
import           Data.Aeson ( ToJSON, decode, encode )
import           Data.Aeson.Types ( Object )
import qualified Data.ByteString.Lazy.Char8 as BSL8 ( ByteString, putStrLn, toStrict, unpack, drop )
import qualified Data.ByteString.Char8 as BS8 ( pack, breakSubstring, dropWhile, putStrLn )
import           Data.Streaming.Network.Internal ( HostPreference ( Host ) )
import           Data.Maybe ( fromJust )
import qualified Data.Map as M ( empty, lookup, member, insert )
import           Control.Monad.State
import           Control.Concurrent.MVar
import           UrlEncodedFormParsing
import           Slack.Parsing
import           Slack.ToJSON
import           Slack.Settings
import           Slack.UsersData

type SlackMethod = String
type SlackCommand = String
type SlackCommandText = String

data SlashCommandData = SlashCommandData SlackCommand SlackCommandText

botUri = "https://slack.com/api/" 

execSlackBot :: SlackSettings -> IO ()
execSlackBot (SlackSettings botToken (ServerSettings serverIP serverPort) textAnswers) = do
  usersDataMV <- newMVar M.empty
  runSettings (setHost (Host serverIP) defaultSettings) (application botToken textAnswers usersDataMV)

-- Use this to notify Slack about successful data receiving
dataRecieved = WAI.responseLBS status200 [] ""

application :: String -> SlackTextAnswers -> MVar UsersData -> WAI.Application
application botToken textAnswers usersDataMV request respond = do
  putStrLn $ "\nTriggered."
  let reqHeaders = WAI.requestHeaders request
  reqBody <- WAI.strictRequestBody request
  case lookup hContentType reqHeaders of
    Just "application/json" -> do
      let parsedRequest = decode reqBody :: Maybe SlackMsg
      case parsedRequest of
        Nothing -> do
          putStrLn "Unknown JSON data."
          respond $ dataRecieved
        Just slackMsg -> do
          handleSlackMsg slackMsg botToken usersDataMV respond
    Just "application/x-www-form-urlencoded" -> do
      let requestData = parseData reqBody
      case getVal "command" requestData of
        Nothing -> do
          let parsedRequest = decode $ BSL8.drop 8 $ parseHexes reqBody :: Maybe SlackPayload
          case parsedRequest of
            Nothing -> putStrLn "Unknown urlencoded data."
            Just payload -> case payload of
              SlackPayloadButton userId actionId -> do
                usersData <- takeMVar usersDataMV
                putMVar usersDataMV (M.insert userId (False, read actionId) usersData)
                putStrLn $ actionId
              UnknownPayload -> do
                putStrLn "Unknown urlencoded data."
                return ()
          respond $ dataRecieved
        Just command -> do
          let responseURL = fromJust $ getVal "response_url" requestData
          handleSlashCommand (SlashCommandData command responseURL) botToken textAnswers respond
      respond $ dataRecieved
    Nothing -> do
      putStrLn "Unknown content."
      respond $ dataRecieved

handleSlackMsg :: SlackMsg -> String -> MVar UsersData -> (WAI.Response -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
handleSlackMsg slackMsg botToken usersDataMV respond = case slackMsg of
  SlackTextMessage channelId userId textMessage -> do
    usersData <- takeMVar usersDataMV
    let usersData' = case M.member userId usersData of
          True  -> usersData
          False -> M.insert userId (False, 1) usersData
    let (isAskedForRepetitions, repetitionsNum) = fromJust $ M.lookup userId usersData'

    sendAnswerNTimes repetitionsNum "chat.postMessage" botToken (SlackTextMessageJSON channelId textMessage)
    putStrLn "Text message was sent."
    respond $ dataRecieved
  SlackOwnMessage -> do
    putStrLn "Own message."
    respond $ dataRecieved
  SlackChallenge challenge -> respond $ WAI.responseLBS
                                          status200
                                          [(hContentType, "application/json")]
                                          (encode (SlackChallengeJSON challenge))

handleSlashCommand :: SlashCommandData -> String -> SlackTextAnswers -> (WAI.Response -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
handleSlashCommand (SlashCommandData command responseURL) botToken textAnswers respond = do
  case command of
    "/about"  -> sendAnswerToCommand responseURL (TextAnswerJSON    $ aboutText textAnswers)
    "/repeat" -> sendAnswerToCommand responseURL (ButtonsAnswerJSON $ repeatText textAnswers)
    _         -> sendAnswerToCommand responseURL (TextAnswerJSON "There is no handler for this command.")
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

sendAnswerToCommand :: (ToJSON a) => String -> a -> IO (Response ())
sendAnswerToCommand responseUrl slackAnswerToCommand = do
  parseRequest responseUrl
    >>= (\request -> httpNoBody request { method = "POST"
                                        , requestHeaders = [(hContentType, "application/json")]
                                        , requestBody = RequestBodyLBS $ encode slackAnswerToCommand })