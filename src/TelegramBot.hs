{-# LANGUAGE OverloadedStrings #-}

module TelegramBot
  ( runTelegramBot,
  ) where

import           Network.HTTP.Simple
import           Network.HTTP.Client.Internal
import qualified Network.HTTP.Types as HTTP (hContentType)
import qualified Data.ByteString.Lazy as BSL (ByteString)
import           Data.Aeson
import           Data.Aeson.Types (Object)
import           Telegram.Parsing
import           Telegram.ToJSON
import           Data.Maybe (fromJust)

myProxy = Proxy "51.158.108.135" 8811
myUri = "https://api.telegram.org/bot"
myToken = ""
pollingTimeout = "60"

runTelegramBot :: Int -> IO ()
runTelegramBot offset = do
  recievedUpdates <- pollTelegram offset
  case recievedUpdates of
    BadRequest description -> putStrLn $ "TelegramBot error: " ++ description
    TelegramMsgs _ -> do
      handledMsgsNum <- handleUpdates recievedUpdates (0, 0)
      if fst handledMsgsNum == 0
      then putStrLn $ "Din't get any messages."
      else putStrLn $ "Handled " ++ show (fst handledMsgsNum) ++ " messages."
      runTelegramBot $ snd handledMsgsNum

pollTelegram :: Int -> IO TelegramMsgs
pollTelegram offset = do
  request <- parseRequest (myUri ++ myToken ++ "/getUpdates?timeout=" ++ pollingTimeout ++ "&offset=" ++ show (offset + 1))
  response <- httpJSON $ request { responseTimeout = ResponseTimeoutNone
                                 , proxy = Just myProxy}
  return $ (getResponseBody response :: TelegramMsgs)

type LastUpdateId       = Int
type SucceedAnswersSize = Int

handleUpdates :: TelegramMsgs -> (SucceedAnswersSize, LastUpdateId) -> IO (SucceedAnswersSize, LastUpdateId)
handleUpdates (TelegramMsgs [])         info                  = return info -- fst - size, snd - update id
handleUpdates (TelegramMsgs (msg:rest)) (sizeNum, lastUpdate) = do
  case msgText msg of
    Just msgText' -> do
      let answer = setRequestProxy (Just myProxy)
            $ parseRequest_ (myUri ++ myToken ++ "/sendMessage")
      response <- httpJSON $ answer { method = "POST"
                                    , requestBody = RequestBodyLBS $ encode $ composeTextMsgRequest msg
                                    , requestHeaders = [(HTTP.hContentType, "application/json")] } :: IO (Response Object)
      handleUpdates (TelegramMsgs rest) (sizeNum + 1, updateId msg)
    Nothing ->
      case sticker msg of
        Just sticker' -> do
          let answer = setRequestProxy (Just myProxy)
                $ parseRequest_ (myUri ++ myToken ++ "/sendSticker")
          response <- httpJSON $ answer { method = "POST"
                                        , requestBody = RequestBodyLBS $ encode $ TelegramStickerJSON (chatId msg) (stickerUniqueId sticker')
                                        , requestHeaders = [(HTTP.hContentType, "application/json")]} :: IO (Response Object)
          handleUpdates (TelegramMsgs rest) (sizeNum + 1, updateId msg)
        Nothing -> do
          return (1, updateId msg)

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
          composeEntityText entityText' "bold"       _         = "*" ++ entityText' ++ "*"
          composeEntityText entityText' "italic"     _         = "_" ++ entityText' ++ "_"
          composeEntityText entityText' _            _         = entityText'