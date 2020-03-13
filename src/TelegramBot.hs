{-# LANGUAGE OverloadedStrings #-}

module TelegramBot
  ( runTelegramBot,
  ) where

import           Network.HTTP.Simple
import           Network.HTTP.Client.Internal
import qualified Network.HTTP.Types as HTTP ( hContentType )
import qualified Data.ByteString.Lazy as BSL ( ByteString )
import           Data.Aeson
import           Data.Aeson.Types ( Object )
import           Telegram.Parsing
import           Telegram.ToJSON
import           Telegram.Settings
import           Data.Maybe ( fromJust )

botUri = "https://api.telegram.org/bot"

runTelegramBot :: Int -> TelegramSettings -> IO ()
runTelegramBot offset settings = do
  recievedUpdates <- pollTelegram offset settings
  case recievedUpdates of
    BadRequest description -> putStrLn $ "TelegramBot error: " ++ description
    TelegramMsgs _ -> do
      handledMsgsNum <- handleUpdates recievedUpdates settings (0, 0)
      if fst handledMsgsNum == 0
      then putStrLn $ "Didn't get any messages."
      else putStrLn $ "Handled " ++ show (fst handledMsgsNum) ++ " messages."
      runTelegramBot (snd handledMsgsNum) settings

pollTelegram :: Int -> TelegramSettings -> IO TelegramMsgs
pollTelegram offset s = do
  request <- parseRequest (botUri ++ botToken s ++ "/getUpdates?timeout=" ++ show (pollingTimeout s) ++ "&offset=" ++ show (offset + 1))
  response <- httpJSON $ request { responseTimeout = ResponseTimeoutNone
                                 , proxy = proxyServer s }
  return $ (getResponseBody response :: TelegramMsgs)

type SucceedAnswersSize = Int
type LastUpdateId       = Int

handleUpdates :: TelegramMsgs -> TelegramSettings -> (SucceedAnswersSize, LastUpdateId) -> IO (SucceedAnswersSize, LastUpdateId)
handleUpdates (TelegramMsgs [])         s info                  = return info
handleUpdates (TelegramMsgs (msg:rest)) s (sizeNum, lastUpdate) = do
  case msgText msg of
    Just msgText' -> do
      sendAnswerNTimes (repetitionsNum s) "sendMessage" (composeTextMsgRequest msg)
      handleUpdates (TelegramMsgs rest) s (sizeNum + 1, updateId msg)
    Nothing ->
      case sticker msg of
        Just sticker' -> do
          sendAnswerNTimes (repetitionsNum s) "sendSticker" (TelegramStickerJSON (chatId msg) (stickerUniqueId sticker'))
          handleUpdates (TelegramMsgs rest) s (sizeNum + 1, updateId msg)
        Nothing -> do
          return (1, updateId msg)
  where sendAnswerNTimes n telegramMethod answerBody = do
          let answer = setRequestProxy (proxyServer s)
                $ parseRequest_ (botUri ++ botToken s ++ "/" ++ telegramMethod)
          repeatAnswer n answer { method = "POST"
                                , requestBody = RequestBodyLBS $ encode $ answerBody
                                , requestHeaders = [(HTTP.hContentType, "application/json")] }
        repeatAnswer 1 answer = httpJSON answer :: IO (Response Object)
        repeatAnswer n answer = do httpJSON answer :: IO (Response Object); repeatAnswer (n-1) answer

composeTextMsgRequest :: TelegramMsg -> TelegramMsgJSON
composeTextMsgRequest msg =
  case entities msg of
    Nothing        -> TelegramMsgJSON (chatId msg) Nothing (fromJust (msgText msg))
    Just entities' -> TelegramMsgJSON (chatId msg) (Just "Markdown") (parseEntities entities' ("", fromJust (msgText msg)) 0)
    where parseEntities [] (composedText, textRest) _ = composedText ++ textRest
          parseEntities (entity:eRest) (composedText, textRest) parsedLength = 
            let (beforeEntity, eTextAndRest) = splitAt ((offset entity) - parsedLength) textRest
                (entityText, afterEntity) = splitAt (entityLength entity) eTextAndRest
            in parseEntities eRest (composedText ++ beforeEntity ++ composeEntityText entityText (entityType entity) (url entity), afterEntity) (offset entity + entityLength entity)
          composeEntityText entityText' "text_link" (Just url) = "[" ++ entityText' ++ "](" ++ url ++ ")"
          composeEntityText entityText' "bold"      _          = "*" ++ entityText' ++ "*"
          composeEntityText entityText' "italic"    _          = "_" ++ entityText' ++ "_"
          composeEntityText entityText' _           _          = entityText'