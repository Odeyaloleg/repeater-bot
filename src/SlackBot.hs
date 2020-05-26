{-# LANGUAGE OverloadedStrings #-}

module SlackBot
  ( execSlackBot
  ) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.Aeson (ToJSON, decode, encode)
import Data.Aeson.Types (Object)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Streaming.Network.Internal (HostPreference(Host))
import Logger (logDebug, logRelease, logWarning)
import qualified Network.HTTP.Client.Internal as HTTP.Internal
import Network.HTTP.Simple
  ( Response
  , getResponseBody
  , getResponseStatusCode
  , httpJSON
  , httpLBS
  , httpNoBody
  , parseRequest
  )
import Network.HTTP.Types (hContentType, status200)
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Slack.Parsing (AnswerStatus(..), SlackMsg(..), SlackPayload(..))
import Slack.Settings
  ( BotToken
  , RepetitionsNum
  , ServerSettings(..)
  , SlackEnv(..)
  , SlackSettings(..)
  , SlackTextAnswers(..)
  )
import Slack.ToJSON
  ( SlackChallengeJSON(..)
  , SlackCommandAnswerJSON(..)
  , SlackTextMessageJSON(..)
  )
import UrlEncodedFormParsing (getVal, hexesToChars, parseUrlEncoded)
import UsersData (UsersData)

type SlackMethod = String

type RequestPath = BS8.ByteString

type RequestBody = BSL.ByteString

botUri = "https://slack.com/api/"

execSlackBot :: SlackSettings -> IO ()
execSlackBot (SlackSettings botToken (ServerSettings serverIP serverPort) repetitionsNum textAnswers logLevel) = do
  usersDataMV <- newMVar M.empty
  runSettings
    (setPort serverPort $ setHost (Host serverIP) defaultSettings)
    (application
       (SlackEnv botToken textAnswers repetitionsNum logLevel)
       usersDataMV)

-- Use this to notify Slack about successful data receiving
dataRecieved = WAI.responseLBS status200 [] ""

application :: SlackEnv -> MVar (UsersData String) -> WAI.Application
application sEnv usersDataMV request respond = do
  let path = WAI.rawPathInfo request
  reqBody <- WAI.strictRequestBody request
  runReaderT (handleRequest path reqBody usersDataMV) sEnv >>= respond

handleRequest ::
     RequestPath
  -> RequestBody
  -> MVar (UsersData String)
  -> ReaderT SlackEnv IO WAI.Response
handleRequest path reqBody usersDataMV =
  case path of
    "/" -> handleTextMsg reqBody usersDataMV
    "/slash_command/" -> handleSlashCommand reqBody
    "/interactivity/" -> handleInteractivity reqBody usersDataMV
    _ -> do
      logRelease $
        BSL.concat
          [ "Slack sent request to unknown path :"
          , BSL.fromStrict path
          , ". Body: "
          , reqBody
          ]
      lift $ putStrLn $ "Requested data from unknown path: " ++ BS8.unpack path
      return dataRecieved

handleTextMsg ::
     BSL.ByteString
  -> MVar (UsersData String)
  -> ReaderT SlackEnv IO WAI.Response
handleTextMsg reqBody usersDataMV = do
  let parsedRequest = decode reqBody :: Maybe SlackMsg
  case parsedRequest of
    Nothing -> do
      logRelease $
        "Couldn't parse text message from Slack. Body: " `BSL.append` reqBody
      lift $ putStrLn "Slack: Unknown data."
      return dataRecieved
    Just slackMsg -> do
      logDebug $ "Recieved text message from Slack: " `BSL.append` reqBody
      defaultRepetitions <- asks repetitionsNum
      case slackMsg of
        SlackTextMessage channelId userId textMessage -> do
          token <- asks botToken
          usersData <- lift $ takeMVar usersDataMV
          let usersData' =
                case M.member userId usersData of
                  True -> usersData
                  False -> M.insert userId (False, defaultRepetitions) usersData
          lift $ putMVar usersDataMV usersData'
          let (isAskedForRepetitions, repetitions) =
                fromJust $ M.lookup userId usersData'
          logDebug "Sending message back."
          answers <-
            sendAnswerNTimes
              repetitions
              "chat.postMessage"
              (SlackTextMessageJSON channelId textMessage)
          let failedSize = length $ filter (== AnswerFail) answers
          let succeedSize = length $ filter (== AnswerSuccess) answers
          if failedSize > 0
            then logRelease $
                 BSL8.concat
                   [ "Failed to send "
                   , (BSL8.pack $ show failedSize)
                   , "/"
                   , (BSL8.pack $ show succeedSize)
                   , " messages."
                   ]
            else logDebug $
                 BSL8.concat
                   [ "Successfully sent all "
                   , (BSL8.pack $ show succeedSize)
                   , " messages."
                   ]
          return dataRecieved
        SlackOwnMessage -> do
          logDebug "Recieved message is own. Actions are not required."
          return dataRecieved
        SlackChallenge challenge -> do
          logRelease "Identification."
          return $
            WAI.responseLBS
              status200
              [(hContentType, "application/json")]
              (encode (SlackChallengeJSON challenge))

handleSlashCommand :: BSL.ByteString -> ReaderT SlackEnv IO WAI.Response
handleSlashCommand reqBody = do
  let requestData = parseUrlEncoded reqBody
  case getVal "command" requestData of
    Nothing -> do
      logRelease $ "Couldn't parse slack command. Body: " `BSL.append` reqBody
      lift $ putStrLn "Slack: Unknown data."
      return dataRecieved
    Just command -> do
      let responseURL = fromJust $ getVal "response_url" requestData
      case command of
        "/about" -> do
          logDebug "Sending answer onto \"/about\" command."
          msgText <- asks $ aboutText . textAnswers
          sendAnswerToCommand responseURL (TextAnswerJSON $ msgText)
        "/repeat" -> do
          logDebug "Sending answer onto \"/repeat\" command."
          msgText <- asks $ repeatText . textAnswers
          sendAnswerToCommand responseURL (ButtonsAnswerJSON $ msgText)
        unknownCommand -> do
          logRelease $
            "Recieved slash command without handler: " `BSL.append`
            BSL8.pack command
          sendAnswerToCommand
            responseURL
            (TextAnswerJSON "There is no handler for this command.")
      return dataRecieved

handleInteractivity ::
     BSL.ByteString
  -> MVar (UsersData String)
  -> ReaderT SlackEnv IO WAI.Response
handleInteractivity reqBody usersDataMV = do
  let requestData = parseUrlEncoded reqBody
  -- Drop first 8 letters, because of format "Payload-<data>"
  let parsedRequest =
        decode $ hexesToChars $ BSL8.drop 8 reqBody :: Maybe SlackPayload
  case parsedRequest of
    Nothing -> do
      logRelease $
        "Couldn't parse interactivity data. Body: " `BSL.append` reqBody
      lift $ putStrLn "Slack: Unknown data."
    Just payload -> do
      case payload of
        SlackPayloadButton userId actionId -> do
          logDebug $
            "Unknown interactivity payload. Body: " `BSL.append` reqBody
          usersData <- lift $ takeMVar usersDataMV
          lift $
            putMVar
              usersDataMV
              (M.insert userId (False, read actionId) usersData)
        UnknownPayload -> do
          logRelease $
            "Unknown interactivity payload. Body: " `BSL.append` reqBody
          lift $ putStrLn "Slack: Unknown data."
  return dataRecieved

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
      let answerStatus = (getResponseBody response :: AnswerStatus)
      return $ answerStatus : answersStatus
    repeatAnswer n answer answersStatus = do
      response <- httpJSON answer
      let answerStatus = (getResponseBody response :: AnswerStatus)
      repeatAnswer (n - 1) answer (answerStatus : answersStatus)

sendAnswerToCommand :: (ToJSON a) => String -> a -> ReaderT SlackEnv IO ()
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
    200 -> logDebug "Successfully sent answer onto command."
    code ->
      logRelease $
      "Couldn't send answer onto command, status code: " `BSL.append`
      BSL8.pack (show code)
