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
import Slack.Parsing (SlackMsg(..), SlackPayload(..))
import Slack.Settings
  ( BotToken
  , RepetitionsNum
  , ServerSettings(..)
  , SlackSettings(..)
  , SlackTextAnswers(..)
  , SlackEnv(..)
  )
import Slack.ToJSON
  ( SlackChallengeJSON(..)
  , SlackCommandAnswerJSON(..)
  , SlackTextMessageJSON(..)
  )
import UrlEncodedFormParsing (getVal, hexesToChars, parseUrlEncoded)
import UsersData (UsersData)

type SlackMethod = String

type SlackCommand = String

type SlackCommandText = String

data SlashCommandData =
  SlashCommandData SlackCommand SlackCommandText

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
          runReaderT (handleSlackMsg slackMsg usersDataMV) sEnv >>= respond
    "/slash_command/" -> do
      let requestData = parseUrlEncoded reqBody
      case getVal "command" requestData of
        Nothing -> do
          putStrLn $ "Slack: Unknown data."
          respond $ dataRecieved
        Just command -> do
          let responseURL = fromJust $ getVal "response_url" requestData
          runReaderT
            (handleSlashCommand (SlashCommandData command responseURL))
            sEnv
          respond $ dataRecieved
    "/interactivity/" -> do
      let requestData = parseUrlEncoded reqBody
      let parsedRequest =
            decode $ hexesToChars $ BSL8.drop 8 reqBody :: Maybe SlackPayload
      case parsedRequest of
        Nothing -> do
          putStrLn "Slack: Unknown data."
        Just payload -> do
          handleInteractivity payload usersDataMV
      respond $ dataRecieved
    _ -> do
      putStrLn $ "Requested data from unknown path: " ++ BS8.unpack path
      respond $ dataRecieved

handleSlackMsg ::
     SlackMsg -> MVar (UsersData String) -> ReaderT SlackEnv IO WAI.Response
handleSlackMsg slackMsg usersDataMV = do
  logLvl <- asks logLevel
  repetitions <- asks repetitionsNum
  case slackMsg of
    SlackTextMessage channelId userId textMessage -> do
      token <- asks botToken
      usersData <- lift $ takeMVar usersDataMV
      let usersData' =
            case M.member userId usersData of
              True -> usersData
              False -> M.insert userId (False, repetitions) usersData
      lift $ putMVar usersDataMV usersData'
      let (isAskedForRepetitions, repetitionsNum) =
            fromJust $ M.lookup userId usersData'
      sendAnswerNTimes
        repetitionsNum
        "chat.postMessage"
        (SlackTextMessageJSON channelId textMessage)
      lift $ putStrLn "Text message was sent."
      return $ dataRecieved
    SlackOwnMessage -> do
      lift $ putStrLn "Own message."
      return $ dataRecieved
    SlackChallenge challenge -> do
      return $
        WAI.responseLBS
          status200
          [(hContentType, "application/json")]
          (encode (SlackChallengeJSON challenge))

handleSlashCommand :: SlashCommandData -> ReaderT SlackEnv IO (Response ())
handleSlashCommand (SlashCommandData command responseURL) = do
  logLvl <- asks logLevel
  case command of
    "/about" -> do
      msgText <- asks $ aboutText . textAnswers
      lift $ sendAnswerToCommand responseURL (TextAnswerJSON $ msgText)
    "/repeat" -> do
      msgText <- asks $ repeatText . textAnswers
      lift $ sendAnswerToCommand responseURL (ButtonsAnswerJSON $ msgText)
    unknownCommand -> do
      lift $
        sendAnswerToCommand
          responseURL
          (TextAnswerJSON "There is no handler for this command.")

handleInteractivity :: SlackPayload -> MVar (UsersData String) -> IO ()
handleInteractivity payload usersDataMV =
  case payload of
    SlackPayloadButton userId actionId -> do
      usersData <- takeMVar usersDataMV
      putMVar usersDataMV (M.insert userId (False, read actionId) usersData)
      putStrLn $ actionId
    UnknownPayload -> do
      putStrLn "Slack: Unknown data."

sendAnswerNTimes ::
     (ToJSON a)
  => Int
  -> SlackMethod
  -> a
  -> ReaderT SlackEnv IO (Response Object)
sendAnswerNTimes n slackMethod slackMsg = do
  token <- asks botToken
  logLvl <- asks logLevel
  answer <- lift $ parseRequest $ botUri ++ slackMethod
  lift $
    repeatAnswer
      n
      answer
        { method = "POST"
        , requestHeaders =
            [ (hContentType, "application/json")
            , ("Authorization", BS8.pack ("Bearer " ++ token))
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
