{-# LANGUAGE OverloadedStrings #-}

module TelegramBot
  ( runTelegramBot,
  ) where

import           Network.HTTP.Simple
import           Network.HTTP.Client.Internal
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Lazy as BSL
import           Data.Aeson
import           Data.Aeson.Types (Object)
import           Parsing.Telegram
import           ToJSON.Telegram
import           Data.Maybe (fromJust)

myProxy = Proxy "51.158.108.135" 8811
myUri = "https://api.telegram.org/bot"
myToken = ""
pollingTimeout = "60"

runTelegramBot :: Integer -> IO ()
runTelegramBot offset = do
  recievedUpdates <- pollTelegram offset
  case recievedUpdates of
    BadRequest description -> error description -- TODO: I believe I red this is a bad way to throw exception. Will take a look later. 
    TelegramMsgs _ -> do
      handledMsgsNum <- handleUpdates recievedUpdates (0, 0)
      if fst handledMsgsNum == 0
      then putStrLn $ "Din't get any messages."
      else putStrLn $ "Handled " ++ show (fst handledMsgsNum) ++ " messages."
      runTelegramBot $ snd handledMsgsNum

pollTelegram :: Integer -> IO TelegramMsgs
pollTelegram offset = do
  request <- parseRequest (myUri ++ myToken ++ "/getUpdates?timeout=" ++ pollingTimeout ++ "&offset=" ++ show (offset + 1))
  response <- httpJSON $ request { responseTimeout = ResponseTimeoutNone
                                 , proxy = Just myProxy}
  return $ (getResponseBody response :: TelegramMsgs)

handleUpdates :: TelegramMsgs -> (Int, Integer) -> IO (Int, Integer)
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
            let (beforeEntity, rest') = splitAt ((offset entity) - parsedLength) textRest
                (entityText, rest'') = splitAt (entityLength entity) rest'
            in parseEntities eRest (composedText ++ beforeEntity ++ composeEntityText entityText (entityType entity) (url entity), rest'') (offset entity + entityLength entity)
          composeEntityText entityText' "text_link" (Just url) = "[" ++ entityText' ++ "](" ++ url ++ ")"
          composeEntityText entityText' _ _ = entityText'