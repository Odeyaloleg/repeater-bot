{-# LANGUAGE OverloadedStrings #-}

module TelegramBot
  ( execTelegramBot
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Aeson (ToJSON, encode)
import Data.Aeson.Types (Object)
import qualified Data.Map as M (empty, insert, lookup, member)
import Data.Maybe (fromJust)
import Network.HTTP.Client.Internal
  ( RequestBody(RequestBodyLBS)
  , Response
  , ResponseTimeout(ResponseTimeoutNone)
  , method
  , parseRequest
  , proxy
  , requestBody
  , requestHeaders
  , responseTimeout
  )
import Network.HTTP.Simple (getResponseBody, httpJSON)
import qualified Network.HTTP.Types as HTTP (hContentType)
import Telegram.Parsing
import Telegram.Settings
import Telegram.ToJSON
import UsersData

type SucceedAnswersSize = Int

type LastUpdateId = Int

type TelegramMethod = String

data HandlerData =
  HandlerData SucceedAnswersSize LastUpdateId UsersData

botUri = "https://api.telegram.org/bot"

execTelegramBot :: TelegramSettings -> IO ()
execTelegramBot s = runReaderT (runTelegramBot M.empty 0) s

runTelegramBot :: UsersData -> LastUpdateId -> ReaderT TelegramSettings IO ()
runTelegramBot d lastUpdId = do
  s <- ask
  recievedUpdates <- pollTelegram lastUpdId
  case recievedUpdates of
    BadRequest description ->
      lift $ putStrLn $ "TelegramBot error: " ++ description
    TelegramUpdates updates -> do
      (HandlerData x y d) <- handleUpdates updates (HandlerData 0 0 d)
      if x == 0
        then lift $
             putStrLn $
             "Telegram: Didn't get any messages possible for handling."
        else lift $
             putStrLn $
             "Telegram: Handled " ++
             show x ++ "/" ++ show (length updates) ++ " messages."
      runTelegramBot d y

pollTelegram :: LastUpdateId -> ReaderT TelegramSettings IO TelegramUpdates
pollTelegram lastUpdId = do
  s <- ask
  let offset = lastUpdId + 1
  request <-
    parseRequest $
    botUri ++
    (botToken (requestSettings s)) ++
    "/getUpdates?timeout=" ++
    show (pollingTimeout s) ++ "&offset=" ++ show offset
  response <-
    httpJSON $
    request
      { responseTimeout = ResponseTimeoutNone
      , proxy = proxyServer (requestSettings s)
      }
  return $ (getResponseBody response :: TelegramUpdates)

handleUpdates ::
     [TelegramMsgUpdate]
  -> HandlerData
  -> ReaderT TelegramSettings IO HandlerData
handleUpdates [] handlerData = return handlerData
handleUpdates ((TelegramMsgUpdate updateId chatId update):rest) (HandlerData succeedAnswersSize lastUpdateId d) = do
  s <- ask
  let usersData =
        case M.member chatId d of
          True -> d
          False -> M.insert chatId (False, (repeatsNum s)) d
  let (isAskedForRepetitions, repetitionsNum) =
        fromJust $ M.lookup chatId usersData
  case (update, isAskedForRepetitions) of
    (TextMsg textMsg _, True) -> do
      result <- lift $ readRepetitions textMsg
      case result of
        Nothing -> do
          lift $
            sendAnswerNTimes
              1
              "sendMessage"
              (requestSettings s)
              (TelegramMsgJSON
                 chatId
                 Nothing
                 "Incorrent answer. Please choose one of the buttons or input digit from 1 to 5."
                 Nothing)
          handleUpdates
            rest
            (HandlerData (succeedAnswersSize + 1) updateId usersData)
        Just n -> do
          lift $
            sendAnswerNTimes
              1
              "sendMessage"
              (requestSettings s)
              (TelegramMsgJSON
                 chatId
                 Nothing
                 ("I will repeat your messages " ++ show n ++ " times.")
                 (Just TelegramKBRemove))
          handleUpdates
            rest
            (HandlerData
               (succeedAnswersSize + 1)
               updateId
               (M.insert chatId (False, n) d))
    (_, True) -> do
      lift $
        sendAnswerNTimes
          1
          "sendMessage"
          (requestSettings s)
          (TelegramMsgJSON
             chatId
             Nothing
             "Incorrent answer. Please choose one of the buttons or input digit from 1 to 5."
             Nothing)
      handleUpdates
        rest
        (HandlerData (succeedAnswersSize + 1) updateId usersData)
    (TextMsg textMsg entities, _) ->
      if head textMsg == '/'
        then do
          lift $ sendAnswerToCommand textMsg repetitionsNum s
          if textMsg == "/repeat"
            then handleUpdates
                   rest
                   (HandlerData
                      (succeedAnswersSize + 1)
                      updateId
                      (M.insert chatId (True, repetitionsNum) usersData))
            else handleUpdates
                   rest
                   (HandlerData (succeedAnswersSize + 1) updateId usersData)
        else do
          lift $
            sendAnswerNTimes
              repetitionsNum
              "sendMessage"
              (requestSettings s)
              (composeTextMsgRequest chatId textMsg entities)
          handleUpdates
            rest
            (HandlerData (succeedAnswersSize + 1) updateId usersData)
    (StickerMsg fileId, _) -> do
      lift $
        sendAnswerNTimes
          repetitionsNum
          "sendSticker"
          (requestSettings s)
          (TelegramStickerJSON chatId fileId)
      handleUpdates
        rest
        (HandlerData (succeedAnswersSize + 1) updateId usersData)
    (UnknownMsg, _) -> do
      lift $ putStrLn "Unknown Message"
      handleUpdates rest (HandlerData succeedAnswersSize updateId usersData)
  where
    sendAnswerToCommand commandText repetitionsNum s = do
      let answerMsg =
            case commandText of
              "/help" -> TelegramMsgJSON chatId Nothing (helpMessage s) Nothing
              "/repeat" ->
                TelegramMsgJSON
                  chatId
                  Nothing
                  (repeatMessage s ++
                   " Now I am repeating your messages " ++
                   show repetitionsNum ++ " times.")
                  (Just
                     (TelegramKBMarkup
                        [ [ TelegramKBButton "1"
                          , TelegramKBButton "2"
                          , TelegramKBButton "3"
                          , TelegramKBButton "4"
                          , TelegramKBButton "5"
                          ]
                        ]))
              commandText' ->
                TelegramMsgJSON
                  chatId
                  Nothing
                  ("Unknown command " ++ commandText' ++ ".")
                  Nothing
      sendAnswerNTimes 1 "sendMessage" (requestSettings s) answerMsg
    readRepetitions t = do
      res <- try $ return $ read t :: IO (Either SomeException Int)
      case res of
        Right n ->
          if n > 0 && n < 6
            then return (Just n)
            else return Nothing
        Left _ -> return Nothing

sendAnswerNTimes ::
     (ToJSON a)
  => Int
  -> TelegramMethod
  -> RequestSettings
  -> a
  -> IO (Response Object)
sendAnswerNTimes n telegramMethod s answerBody = do
  answer <- parseRequest $ botUri ++ botToken s ++ "/" ++ telegramMethod
  repeatAnswer
    n
    answer
      { method = "POST"
      , proxy = proxyServer s
      , requestHeaders = [(HTTP.hContentType, "application/json")]
      , requestBody = RequestBodyLBS $ encode $ answerBody
      }
  where
    repeatAnswer 1 answer = httpJSON answer :: IO (Response Object)
    repeatAnswer n answer = do
      httpJSON answer :: IO (Response Object)
      repeatAnswer (n - 1) answer

composeTextMsgRequest ::
     ChatId -> String -> Maybe [TelegramEntity] -> TelegramMsgJSON
composeTextMsgRequest chatId textMsg entities =
  case entities of
    Nothing -> TelegramMsgJSON chatId Nothing textMsg Nothing
    Just entities' ->
      TelegramMsgJSON
        chatId
        (Just "Markdown")
        (parseEntities entities' ("", textMsg) 0)
        Nothing
  where
    parseEntities [] (composedText, notParsed) _ = composedText ++ notParsed
    parseEntities ((TelegramEntity offset eLength eType eUrl):eRest) (composedText, notParsed) parsedLength =
      let (beforeEntity, eTextAndRest) =
            splitAt (offset - parsedLength) notParsed
          (entityText, afterEntity) = splitAt eLength eTextAndRest
       in parseEntities
            eRest
            ( composedText ++
              beforeEntity ++ composeEntityText entityText eType eUrl
            , afterEntity)
            (offset + eLength)
    composeEntityText entityText' "text_link" (Just url) =
      "[" ++ entityText' ++ "](" ++ url ++ ")"
    composeEntityText entityText' "bold" _ = "*" ++ entityText' ++ "*"
    composeEntityText entityText' "italic" _ = "_" ++ entityText' ++ "_"
    composeEntityText entityText' _ _ = entityText'
