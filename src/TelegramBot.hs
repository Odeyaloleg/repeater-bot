{-# LANGUAGE OverloadedStrings #-}

module TelegramBot
  ( execTelegramBot
  ) where

import           Network.HTTP.Simple
import           Network.HTTP.Client.Internal
import qualified Network.HTTP.Types as HTTP ( hContentType )
import qualified Data.ByteString.Lazy as BSL ( ByteString, fromStrict )
import           Data.Aeson
import           Data.Aeson.Types ( Object )
import           Data.Maybe ( fromJust )
import qualified Data.Map as M ( empty, lookup, member, insert )
import           Control.Exception ( SomeException, try )
import           Telegram.Parsing
import           Telegram.ToJSON
import           Telegram.Settings
import           Telegram.UsersData

botUri = "https://api.telegram.org/bot"

execTelegramBot :: TelegramSettings -> IO ()
execTelegramBot s = runTelegramBot s (UsersData M.empty) 0

runTelegramBot :: TelegramSettings -> UsersData -> Int -> IO ()
runTelegramBot s d offset = do
  recievedUpdates <- pollTelegram s offset
  case recievedUpdates of
    BadRequest description -> putStrLn $ "TelegramBot error: " ++ description
    TelegramMsgs _ -> do
      (sizeNum, lastUpdate, usersData) <- handleUpdates recievedUpdates s (0, 0, d)
      if sizeNum == 0
      then putStrLn $ "Didn't get any messages."
      else putStrLn $ "Handled " ++ show sizeNum ++ " messages."
      runTelegramBot s usersData lastUpdate

pollTelegram :: TelegramSettings -> Int -> IO TelegramMsgs
pollTelegram s offset = do
  request <- parseRequest (botUri ++ botToken s ++ "/getUpdates?timeout=" ++ show (pollingTimeout s) ++ "&offset=" ++ show (offset + 1))
  response <- httpJSON $ request { responseTimeout = ResponseTimeoutNone
                                 , proxy = proxyServer s }
  return $ (getResponseBody response :: TelegramMsgs)

type SucceedAnswersSize = Int
type LastUpdateId       = Int

handleUpdates :: TelegramMsgs -> TelegramSettings -> (SucceedAnswersSize, LastUpdateId, UsersData) -> IO (SucceedAnswersSize, LastUpdateId, UsersData)
handleUpdates (TelegramMsgs [])         s info                                 = return info
handleUpdates (TelegramMsgs (msg:rest)) s (sizeNum, lastUpdate, (UsersData d)) = do
  let usersData = case M.member (chatId msg) d of
        True  -> d
        False -> M.insert (chatId msg) (False, (repeatsNum s)) d
  let (isAskedForRepetitions, repetitionsNum) = fromJust $ M.lookup (chatId msg) usersData
  case msgText msg of
    Just msgText' -> do
      if isAskedForRepetitions == True
        then do
          result <- readRepetitions msgText'
          case result of
            Nothing -> do
              sendAnswerNTimes 1 "sendMessage" (TelegramMsgJSON (chatId msg) Nothing "Incorrent answer. Please choose one of the buttons or input digit from 1 to 5." Nothing)
              handleUpdates (TelegramMsgs rest) s (sizeNum + 1, updateId msg, (UsersData usersData))
            Just n  -> do
              sendAnswerNTimes 1 "sendMessage" (TelegramMsgJSON (chatId msg) Nothing ("I will repeat your messages " ++ show n ++ " times.") (Just TelegramKBRemove))
              handleUpdates (TelegramMsgs rest) s (sizeNum + 1, updateId msg, (UsersData (M.insert (chatId msg) (False, n) d)))
        else do
          if head msgText' == '/'
            then do
              sendAnswerToCommand msgText'
              if msgText' == "/repeat"
                then handleUpdates (TelegramMsgs rest) s (sizeNum + 1, updateId msg, UsersData (M.insert (chatId msg) (True, repetitionsNum) usersData))
                else handleUpdates (TelegramMsgs rest) s (sizeNum + 1, updateId msg, (UsersData usersData))
            else do
              sendAnswerNTimes repetitionsNum "sendMessage" (composeTextMsgRequest msg)
              handleUpdates (TelegramMsgs rest) s (sizeNum + 1, updateId msg, (UsersData usersData))
    Nothing ->
      case sticker msg of
        Just sticker' -> do
          sendAnswerNTimes repetitionsNum "sendSticker" (TelegramStickerJSON (chatId msg) (stickerUniqueId sticker'))
          handleUpdates (TelegramMsgs rest) s (sizeNum + 1, updateId msg, (UsersData usersData))
        Nothing -> do
          return (sizeNum, updateId msg, (UsersData usersData))
  where sendAnswerNTimes n telegramMethod answerBody = do
          let answer = setRequestProxy (proxyServer s)
                $ parseRequest_ (botUri ++ botToken s ++ "/" ++ telegramMethod)
          repeatAnswer n answer { method = "POST"
                                , requestBody = RequestBodyLBS $ encode $ answerBody
                                , requestHeaders = [(HTTP.hContentType, "application/json")] }
        repeatAnswer 1 answer = httpJSON answer :: IO (Response Object)
        repeatAnswer n answer = do httpJSON answer :: IO (Response Object); repeatAnswer (n-1) answer
        sendAnswerToCommand commandText = do
          let answerText = case commandText of
                "/help"   -> encode $ TelegramMsgJSON (chatId msg) Nothing (helpMessage s) Nothing
                "/repeat" -> encode $ TelegramMsgJSON (chatId msg) Nothing (repeatMessage s) (Just (TelegramKBMarkup [[TelegramKBButton "1",TelegramKBButton "2",TelegramKBButton "3",TelegramKBButton "4",TelegramKBButton "5"]]))
                commandText' -> encode $ TelegramMsgJSON (chatId msg) Nothing ("Unknown command " ++ commandText' ++ ".") Nothing
          let answer = setRequestProxy (proxyServer s)
                $ parseRequest_ (botUri ++ botToken s ++ "/sendMessage")
          httpJSON $ answer { method = "POST"
                            , requestBody = RequestBodyLBS $ answerText
                            , requestHeaders = [(HTTP.hContentType, "application/json")] } :: IO (Response Object)
        readRepetitions t = do
          res <- try $ return $ read t :: IO (Either SomeException Int)
          case res of
            Right n -> if n > 0 && n < 6 then return (Just n) else return Nothing
            Left _  -> return Nothing

composeTextMsgRequest :: TelegramMsg -> TelegramMsgJSON
composeTextMsgRequest msg =
  case entities msg of
    Nothing        -> TelegramMsgJSON (chatId msg) Nothing (fromJust (msgText msg)) Nothing
    Just entities' -> TelegramMsgJSON (chatId msg) (Just "Markdown") (parseEntities entities' ("", fromJust (msgText msg)) 0) Nothing
    where parseEntities [] (composedText, textRest) _ = composedText ++ textRest
          parseEntities (entity:eRest) (composedText, textRest) parsedLength = 
            let (beforeEntity, eTextAndRest) = splitAt ((offset entity) - parsedLength) textRest
                (entityText, afterEntity) = splitAt (entityLength entity) eTextAndRest
            in parseEntities eRest (composedText ++ beforeEntity ++ composeEntityText entityText (entityType entity) (url entity), afterEntity) (offset entity + entityLength entity)
          composeEntityText entityText' "text_link" (Just url) = "[" ++ entityText' ++ "](" ++ url ++ ")"
          composeEntityText entityText' "bold"      _          = "*" ++ entityText' ++ "*"
          composeEntityText entityText' "italic"    _          = "_" ++ entityText' ++ "_"
          composeEntityText entityText' _           _          = entityText'