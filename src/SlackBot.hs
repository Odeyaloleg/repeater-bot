{-# LANGUAGE OverloadedStrings #-}

module SlackBot
  ( execSlackBot
  ) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.Aeson (ToJSON, decode, encode)
import Data.Aeson.Types (Object)
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map as M (empty, insert, lookup, member)
import Data.Maybe (fromJust)
import Data.Streaming.Network.Internal (HostPreference(Host))
import Logging (LogLevel(..), logSlack)
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

data SlackEnv =
  SlackEnv
    { botToken :: BotToken
    , textAnswers :: SlackTextAnswers
    , repetitionsNum :: RepetitionsNum
    , logLevel :: LogLevel
    }

botUri = "https://slack.com/api/"

execSlackBot :: SlackSettings -> IO ()
execSlackBot (SlackSettings botToken (ServerSettings serverIP serverPort) repetitionsNum textAnswers logLevel) = do
  usersDataMV <- newMVar M.empty
  runSettings
    (setPort serverPort $ setHost (Host serverIP) defaultSettings)
    (application (SlackEnv botToken textAnswers repetitionsNum logLevel) usersDataMV)

-- Use this to notify Slack about successful data receiving
dataRecieved = WAI.responseLBS status200 [] ""

application :: SlackEnv -> MVar (UsersData String) -> WAI.Application
application sEnv usersDataMV request respond = do
  putStrLn $ "\nTriggered."
  reqBody <- WAI.strictRequestBody request
  when
    (logLevel sEnv == LevelDEBUG)
    (logSlack ("Request from Slack: " `BSL.append` reqBody))
  let path = WAI.rawPathInfo request
  case path of
    "/" -> do
      let parsedRequest = decode reqBody :: Maybe SlackMsg
      case parsedRequest of
        Nothing -> do
          putStrLn "Slack: Unknown data."
          when
            (logLevel sEnv == LevelDEBUG)
            (logSlack "Couldn't parse request.")
          when
            (logLevel sEnv == LevelWARN)
            (logSlack ("Couldn't parse request: " `BSL.append` reqBody))
          respond $ dataRecieved
        Just slackMsg -> do
          runReaderT (handleSlackMsg slackMsg usersDataMV) sEnv >>= respond
    "/slash_command/" -> do
      let requestData = parseUrlEncoded reqBody
      case getVal "command" requestData of
        Nothing -> do
          putStrLn $ "Slack: Unknown data."
          when
            (logLevel sEnv == LevelDEBUG)
            (logSlack "Unknown data in request.")
          when
            (logLevel sEnv == LevelWARN)
            (logSlack ("Unknown data in request: " `BSL.append` reqBody))
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
          when
            (logLevel sEnv == LevelDEBUG)
            (logSlack "Unknown data in request.")
          when
            (logLevel sEnv == LevelWARN)
            (logSlack ("Unknown data in request: " `BSL.append` reqBody))
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
              when
                (logLevel sEnv == LevelDEBUG)
                (logSlack "Unknown data in request.")
              when
                (logLevel sEnv == LevelWARN)
                (logSlack ("Unknown data in request: " `BSL.append` reqBody))
      respond $ dataRecieved
    _ -> do
      putStrLn $ "Requested data from unknown path: " ++ BS8.unpack path
      when
        (logLevel sEnv < LevelRELEASE)
        (logSlack
           ("Requested data from unknown path: " `BSL.append`
            BSL.fromStrict path))
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
      lift $ when (logLvl == LevelDEBUG) (logSlack "Own message delivered.")
      return $ dataRecieved
    SlackChallenge challenge -> do
      lift $ when (logLvl == LevelDEBUG) (logSlack "Sending challenge.")
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
      lift $ when (logLvl < LevelRELEASE) (logSlack "Sending answer on \"/about\" command.")
      msgText <- asks $ aboutText . textAnswers
      lift $ sendAnswerToCommand responseURL (TextAnswerJSON $ msgText)
    "/repeat" -> do
      lift $ when (logLvl < LevelRELEASE) (logSlack "Sending answer on \"/repeat\" command.")
      msgText <- asks $ repeatText . textAnswers
      lift $ sendAnswerToCommand responseURL (ButtonsAnswerJSON $ msgText)
    unknownCommand -> do
      lift $ when (logLvl == LevelDEBUG) (logSlack $ "There is no handler for command " `BSL.append` BSL8.pack unknownCommand)
      lift $
        sendAnswerToCommand
          responseURL
          (TextAnswerJSON "There is no handler for this command.")

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
  lift $ when (logLvl == LevelDEBUG) (logSlack $ "Sending message: " `BSL.append` encode slackMsg)
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
