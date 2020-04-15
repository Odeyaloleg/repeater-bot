{-# LANGUAGE OverloadedStrings #-}

module TelegramBot
  ( execTelegramBot
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import qualified Data.Map as M (empty, insert, lookup, member)
import Data.Maybe (fromJust)
import Network.HTTP.Client.Internal
  ( RequestBody(RequestBodyLBS)
  , ResponseTimeout(ResponseTimeoutNone)
  , parseRequest
  , proxy
  , responseTimeout
  )
import Network.HTTP.Simple (getResponseBody, httpJSON)
import Telegram.Parsing
import Telegram.SendingMessages
import Telegram.Settings
import Telegram.ToJSON
import UsersData

type LastUpdateId = Int

data HandlerData =
  HandlerData [(TelegramBotMsgJSON, Int)] LastUpdateId (UsersData Int)

execTelegramBot :: TelegramSettings -> IO ()
execTelegramBot s = runReaderT (runTelegramBot M.empty 0) s

runTelegramBot :: UsersData Int -> LastUpdateId -> ReaderT TelegramSettings IO ()
runTelegramBot d lastUpdId = do
  recievedUpdates <- pollTelegram lastUpdId
  case recievedUpdates of
    BadRequest description ->
      lift $ putStrLn $ "TelegramBot error: " ++ description
    TelegramUpdates updates -> do
      (HandlerData botMsgs lastUpdateId d) <-
        handleUpdates updates (HandlerData [] 0 d)
      if length botMsgs == 0
        then lift $
             putStrLn $
             "Telegram: Didn't get any messages possible for handling."
        else do
          s <- ask
          lift $ sendMessagesNTimes botMsgs (requestSettings s)
          lift $
            putStrLn $
            "Telegram: Handled " ++
            show (length botMsgs) ++
            "/" ++ show (length updates) ++ " messages."
      runTelegramBot d lastUpdateId

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
handleUpdates ((TelegramMsgUpdate updateId chatId update):rest) (HandlerData botMsgs lastUpdateId d) = do
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
          handleUpdates
            rest
            (HandlerData
               (( TextMsgJSON $
                  BotTextMsgJSON
                    chatId
                    Nothing
                    "Incorrent answer. Please choose one of the buttons or input digit from 1 to 5."
                    Nothing
                , 1) :
                botMsgs)
               updateId
               usersData)
        Just n -> do
          handleUpdates
            rest
            (HandlerData
               (( TextMsgJSON $
                  BotTextMsgJSON
                    chatId
                    Nothing
                    ("I will repeat your messages " ++ show n ++ " times.")
                    (Just TelegramKBRemove)
                , 1) :
                botMsgs)
               updateId
               (M.insert chatId (False, n) d))
    (_, True) -> do
      handleUpdates
        rest
        (HandlerData
           (( TextMsgJSON $
              BotTextMsgJSON
                chatId
                Nothing
                "Incorrent answer. Please choose one of the buttons or input digit from 1 to 5."
                Nothing
            , 1) :
            botMsgs)
           updateId
           usersData)
    (TextMsg textMsg entities, _) -> do
      if head textMsg == '/'
        then do
          let answerMsg =
                case textMsg of
                  "/help" ->
                    TextMsgJSON $
                    BotTextMsgJSON chatId Nothing (helpMessage s) Nothing
                  "/repeat" ->
                    TextMsgJSON $
                    BotTextMsgJSON
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
                    TextMsgJSON $
                    BotTextMsgJSON
                      chatId
                      Nothing
                      ("Unknown command " ++ commandText' ++ ".")
                      Nothing
          if textMsg == "/repeat"
            then handleUpdates
                   rest
                   (HandlerData
                      ((answerMsg, 1) : botMsgs)
                      updateId
                      (M.insert chatId (True, repetitionsNum) usersData))
            else handleUpdates
                   rest
                   (HandlerData ((answerMsg, 1) : botMsgs) updateId usersData)
        else do
          handleUpdates
            rest
            (HandlerData
               ((composeTextMsgRequest chatId textMsg entities, repetitionsNum) :
                botMsgs)
               updateId
               usersData)
    (StickerMsg fileId, _) -> do
      handleUpdates
        rest
        (HandlerData
           ((StickerMsgJSON $ BotStickerMsgJSON chatId fileId, 1) : botMsgs)
           updateId
           usersData)
    (UnknownMsg, _) -> do
      lift $ putStrLn "Unknown Message"
      handleUpdates rest (HandlerData botMsgs updateId usersData)
  where
    readRepetitions t = do
      res <- try $ return $ read t :: IO (Either SomeException Int)
      case res of
        Right n ->
          if n > 0 && n < 6
            then return (Just n)
            else return Nothing
        Left _ -> return Nothing

composeTextMsgRequest ::
     ChatId -> String -> Maybe [TelegramEntity] -> TelegramBotMsgJSON
composeTextMsgRequest chatId textMsg entities =
  case entities of
    Nothing -> TextMsgJSON $ BotTextMsgJSON chatId Nothing textMsg Nothing
    Just entities' ->
      TextMsgJSON $
      BotTextMsgJSON
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
