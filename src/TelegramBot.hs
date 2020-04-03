{-# LANGUAGE OverloadedStrings #-}

module TelegramBot
  ( execTelegramBot
  ) where

import           Network.HTTP.Simple ( httpJSON, getResponseBody )
import           Network.HTTP.Client.Internal ( Response, RequestBody ( RequestBodyLBS ), ResponseTimeout ( ResponseTimeoutNone )
                                              , method, proxy, requestHeaders, requestBody, responseTimeout, parseRequest )
import qualified Network.HTTP.Types as HTTP ( hContentType )
import           Data.Aeson ( ToJSON, encode )
import           Data.Aeson.Types ( Object )
import           Data.Maybe ( fromJust )
import qualified Data.Map as M ( empty, lookup, member, insert )
import           Control.Exception ( SomeException, try )
import           Telegram.Parsing
import           Telegram.ToJSON
import           Telegram.Settings
import           UsersData

type SucceedAnswersSize = Int
type LastUpdateId       = Int
type TelegramMethod     = String

data HandlerData = HandlerData SucceedAnswersSize LastUpdateId UsersData

botUri = "https://api.telegram.org/bot"

execTelegramBot :: TelegramSettings -> IO ()
execTelegramBot s = runTelegramBot s M.empty 0

runTelegramBot :: TelegramSettings -> UsersData -> LastUpdateId -> IO ()
runTelegramBot s d lastUpdId = do
  recievedUpdates <- pollTelegram s lastUpdId
  case recievedUpdates of
    BadRequest description -> putStrLn $ "TelegramBot error: " ++ description
    TelegramUpdates updates -> do
      (HandlerData x y d) <- handleUpdates updates s (HandlerData 0 0 d)
      if x == 0
      then putStrLn $ "Telegram: Didn't get any messages possible for handling."
      else putStrLn $ "Telegram: Handled " ++ show x ++ "/" ++ show (length updates) ++ " messages."
      runTelegramBot s d y

pollTelegram :: TelegramSettings -> LastUpdateId -> IO TelegramUpdates
pollTelegram s lastUpdId = do
  let offset = lastUpdId + 1
  request <- parseRequest $ botUri ++ (botToken (requestSettings s)) ++ "/getUpdates?timeout=" ++ show (pollingTimeout s) ++ "&offset=" ++ show offset
  response <- httpJSON $ request { responseTimeout = ResponseTimeoutNone
                                 , proxy = proxyServer (requestSettings s) }
  return $ (getResponseBody response :: TelegramUpdates)

handleUpdates :: [TelegramMsgUpdate] -> TelegramSettings -> HandlerData -> IO HandlerData
handleUpdates [] _ handlerData = return handlerData
handleUpdates ((TelegramMsgUpdate updateId chatId update):rest) s (HandlerData succeedAnswersSize lastUpdateId d) = do
  let usersData = case M.member chatId d of
        True  -> d
        False -> M.insert chatId (False, (repeatsNum s)) d
  let (isAskedForRepetitions, repetitionsNum) = fromJust $ M.lookup chatId usersData

  case (update,isAskedForRepetitions) of
    (TextMsg textMsg _, True) -> do
      result <- readRepetitions textMsg
      case result of
        Nothing -> do
          sendAnswerNTimes 1 "sendMessage" (requestSettings s) (TelegramMsgJSON chatId Nothing "Incorrent answer. Please choose one of the buttons or input digit from 1 to 5." Nothing)
          handleUpdates rest s (HandlerData (succeedAnswersSize + 1) updateId usersData)
        Just n  -> do
          sendAnswerNTimes 1 "sendMessage" (requestSettings s) (TelegramMsgJSON chatId Nothing ("I will repeat your messages " ++ show n ++ " times.") (Just TelegramKBRemove))
          handleUpdates rest s (HandlerData (succeedAnswersSize + 1) updateId (M.insert chatId (False, n) d))
    (_, True) -> do
      sendAnswerNTimes 1 "sendMessage" (requestSettings s) (TelegramMsgJSON chatId Nothing "Incorrent answer. Please choose one of the buttons or input digit from 1 to 5." Nothing)
      handleUpdates rest s (HandlerData (succeedAnswersSize + 1) updateId usersData)
    (TextMsg textMsg entities, _) ->
      if head textMsg == '/'
        then do
          sendAnswerToCommand textMsg repetitionsNum
          if textMsg == "/repeat"
            then handleUpdates rest s (HandlerData (succeedAnswersSize + 1) updateId (M.insert chatId (True, repetitionsNum) usersData))
            else handleUpdates rest s (HandlerData (succeedAnswersSize + 1) updateId usersData)
        else do
          sendAnswerNTimes repetitionsNum "sendMessage" (requestSettings s) (composeTextMsgRequest chatId textMsg entities)
          handleUpdates rest s (HandlerData (succeedAnswersSize + 1) updateId usersData)
    (StickerMsg fileId, _) -> do
      sendAnswerNTimes repetitionsNum "sendSticker" (requestSettings s) (TelegramStickerJSON chatId fileId)
      handleUpdates rest s (HandlerData (succeedAnswersSize + 1) updateId usersData)
    (UnknownMsg, _) -> do
      putStrLn "Unknown Message"
      handleUpdates rest s (HandlerData succeedAnswersSize updateId usersData)
  where
  sendAnswerToCommand commandText repetitionsNum = do
    let answerMsg = case commandText of
          "/help"      -> TelegramMsgJSON chatId Nothing (helpMessage s) Nothing
          "/repeat"    -> TelegramMsgJSON chatId Nothing (repeatMessage s ++ " Now I am repeating your messages " ++ show repetitionsNum ++ " times.") (Just (TelegramKBMarkup [[TelegramKBButton "1",TelegramKBButton "2",TelegramKBButton "3",TelegramKBButton "4",TelegramKBButton "5"]]))
          commandText' -> TelegramMsgJSON chatId Nothing ("Unknown command " ++ commandText' ++ ".") Nothing
    sendAnswerNTimes 1 "sendMessage" (requestSettings s) answerMsg

  readRepetitions t = do
    res <- try $ return $ read t :: IO (Either SomeException Int)
    case res of
      Right n -> if n > 0 && n < 6 then return (Just n) else return Nothing
      Left _  -> return Nothing

sendAnswerNTimes :: (ToJSON a) => Int -> TelegramMethod -> RequestSettings -> a -> IO (Response Object)
sendAnswerNTimes n telegramMethod s answerBody = do
  answer <- parseRequest $ botUri ++ botToken s ++ "/" ++ telegramMethod
  repeatAnswer n answer { method = "POST"
                        , proxy = proxyServer s
                        , requestHeaders = [(HTTP.hContentType, "application/json")]
                        , requestBody = RequestBodyLBS $ encode $ answerBody }
  where
  repeatAnswer 1 answer = httpJSON answer :: IO (Response Object)
  repeatAnswer n answer = do
    httpJSON answer :: IO (Response Object)
    repeatAnswer (n-1) answer

composeTextMsgRequest :: ChatId -> String -> Maybe [TelegramEntity] -> TelegramMsgJSON
composeTextMsgRequest chatId textMsg entities =
  case entities of
    Nothing        -> TelegramMsgJSON chatId Nothing textMsg Nothing
    Just entities' -> TelegramMsgJSON chatId (Just "Markdown") (parseEntities entities' ("", textMsg) 0) Nothing
  where
  parseEntities [] (composedText, notParsed) _ = composedText ++ notParsed
  parseEntities ((TelegramEntity offset eLength eType eUrl):eRest) (composedText, notParsed) parsedLength = 
    let (beforeEntity, eTextAndRest) = splitAt (offset - parsedLength) notParsed
        (entityText, afterEntity)    = splitAt eLength eTextAndRest
    in parseEntities eRest (composedText ++ beforeEntity ++ composeEntityText entityText eType eUrl, afterEntity) (offset + eLength)
  
  composeEntityText entityText' "text_link" (Just url) = "[" ++ entityText' ++ "](" ++ url ++ ")"
  composeEntityText entityText' "bold"      _          = "*" ++ entityText' ++ "*"
  composeEntityText entityText' "italic"    _          = "_" ++ entityText' ++ "_"
  composeEntityText entityText' _           _          = entityText'