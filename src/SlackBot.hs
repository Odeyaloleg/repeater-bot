{-# LANGUAGE OverloadedStrings #-}

module SlackBot
  ( execSlackBot
  ) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Data.Aeson (ToJSON, decode, encode)
import Data.Aeson.Types (Object)
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL8 (drop)
import qualified Data.Map as M (empty, insert, lookup, member)
import Data.Maybe (fromJust)
import Data.Streaming.Network.Internal (HostPreference(Host))
import Network.HTTP.Client.Internal
  ( RequestBody(RequestBodyLBS)
  , method
  , requestBody
  , requestHeaders
  )
import Network.HTTP.Simple (Response, httpJSON, httpNoBody, parseRequest)
import Network.HTTP.Types (hContentType, status200)
import qualified Network.Wai as WAI hiding (requestBody)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Slack.Parsing
import Slack.Settings
import Slack.ToJSON
import Slack.UsersData
import UrlEncodedFormParsing

type SlackMethod = String

type SlackCommand = String

type SlackCommandText = String

data SlashCommandData =
  SlashCommandData SlackCommand SlackCommandText

botUri = "https://slack.com/api/"

execSlackBot :: SlackSettings -> IO ()
execSlackBot (SlackSettings botToken (ServerSettings serverIP serverPort) textAnswers) = do
  usersDataMV <- newMVar M.empty
  runSettings
    (setPort serverPort $ setHost (Host serverIP) defaultSettings)
    (application botToken textAnswers usersDataMV)

-- Use this to notify Slack about successful data receiving
dataRecieved = WAI.responseLBS status200 [] ""

application :: String -> SlackTextAnswers -> MVar UsersData -> WAI.Application
application botToken textAnswers usersDataMV request respond = do
  putStrLn $ "\nTriggered."
  reqBody <- WAI.strictRequestBody request
  let path = WAI.rawPathInfo request
  case path of
    "/" -> do
      let parsedRequest = decode reqBody :: Maybe SlackMsg
      case parsedRequest of
        Nothing -> do
          putStrLn "Slack: Unknown data."
          respond $ dataRecieved
        Just slackMsg -> do
          handleSlackMsg slackMsg botToken usersDataMV >>= respond
    "/slash_command/" -> do
      let requestData = parseData reqBody
      case getVal "command" requestData of
        Nothing -> do
          putStrLn $ "Slack: Unknown data."
          respond $ dataRecieved
        Just command -> do
          let responseURL = fromJust $ getVal "response_url" requestData
          handleSlashCommand
            (SlashCommandData command responseURL)
            botToken
            textAnswers
          respond $ dataRecieved
    "/interactivity/" -> do
      let requestData = parseData reqBody
      let parsedRequest =
            decode $ parseHexes $ BSL8.drop 8 reqBody :: Maybe SlackPayload
      case parsedRequest of
        Nothing -> do
          putStrLn "Slack: Unknown data."
        Just payload -> do
          case payload of
            SlackPayloadButton userId actionId -> do
              usersData <- takeMVar usersDataMV
              putMVar
                usersDataMV
                (M.insert userId (False, read actionId) usersData)
              putStrLn $ actionId
            UnknownPayload -> do
              putStrLn "Slack: Unknown data."
      respond $ dataRecieved
    _ -> do
      putStrLn $ "Requested data from unknown path: " ++ BS8.unpack path
      respond $ dataRecieved

handleSlackMsg :: SlackMsg -> String -> MVar UsersData -> IO WAI.Response
handleSlackMsg slackMsg botToken usersDataMV =
  case slackMsg of
    SlackTextMessage channelId userId textMessage -> do
      usersData <- takeMVar usersDataMV
      let usersData' =
            case M.member userId usersData of
              True -> usersData
              False -> M.insert userId (False, 1) usersData
      let (isAskedForRepetitions, repetitionsNum) =
            fromJust $ M.lookup userId usersData'
      sendAnswerNTimes
        repetitionsNum
        "chat.postMessage"
        botToken
        (SlackTextMessageJSON channelId textMessage)
      putStrLn "Text message was sent."
      return $ dataRecieved
    SlackOwnMessage -> do
      putStrLn "Own message."
      return $ dataRecieved
    SlackChallenge challenge ->
      return $
      WAI.responseLBS
        status200
        [(hContentType, "application/json")]
        (encode (SlackChallengeJSON challenge))

handleSlashCommand ::
     SlashCommandData -> String -> SlackTextAnswers -> IO (Response ())
handleSlashCommand (SlashCommandData command responseURL) botToken textAnswers = do
  case command of
    "/about" ->
      sendAnswerToCommand responseURL (TextAnswerJSON $ aboutText textAnswers)
    "/repeat" ->
      sendAnswerToCommand
        responseURL
        (ButtonsAnswerJSON $ repeatText textAnswers)
    _ ->
      sendAnswerToCommand
        responseURL
        (TextAnswerJSON "There is no handler for this command.")

sendAnswerNTimes ::
     (ToJSON a) => Int -> SlackMethod -> BotToken -> a -> IO (Response Object)
sendAnswerNTimes n slackMethod botToken slackMsg = do
  answer <- parseRequest $ botUri ++ slackMethod
  repeatAnswer
    n
    answer
      { method = "POST"
      , requestHeaders =
          [ (hContentType, "application/json")
          , ("Authorization", BS8.pack ("Bearer " ++ botToken))
          ]
      , requestBody = RequestBodyLBS $ encode slackMsg
      }
  where
    repeatAnswer 1 answer = httpJSON answer :: IO (Response Object)
    repeatAnswer n answer = do
      httpJSON answer :: IO (Response Object)
      repeatAnswer (n - 1) answer

sendAnswerToCommand :: (ToJSON a) => String -> a -> IO (Response ())
sendAnswerToCommand responseUrl slackAnswerToCommand = do
  parseRequest responseUrl >>=
    (\request ->
       httpNoBody
         request
           { method = "POST"
           , requestHeaders = [(hContentType, "application/json")]
           , requestBody = RequestBodyLBS $ encode slackAnswerToCommand
           })
