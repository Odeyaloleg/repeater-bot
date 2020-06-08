{-# LANGUAGE OverloadedStrings #-}

module Telegram.HandleUpdates where

import Control.Monad.Reader (ReaderT, ask, lift)
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map as Map
import RepeaterBot.Logger (logDebug, logWarning)
import RepeaterBot.UsersData (UsersData)
import Telegram.BotModel (ChatId, LastUpdateId)
import Telegram.Parsing
  ( TelegramEntity(..)
  , TelegramMsg(..)
  , TelegramMsgUpdate(..)
  , TelegramUpdates(..)
  )
import Telegram.SendingMessages (TelegramBotMsgJSON(..))
import Telegram.Settings (TelegramSettings(..))
import Telegram.ToJSON
  ( BotStickerMsgJSON(..)
  , BotTextMsgJSON(..)
  , TelegramKBButton(..)
  , TelegramReplyMarkup(..)
  )
import Text.Read (readMaybe)

type RepetitionsNum = Int

data HandlerData =
  HandlerData
    [(TelegramBotMsgJSON, RepetitionsNum)]
    LastUpdateId
    (UsersData Int)

handleUpdates ::
     [TelegramMsgUpdate]
  -> HandlerData
  -> ReaderT TelegramSettings IO HandlerData
handleUpdates [] handlerData = return handlerData
handleUpdates ((TelegramMsgUpdate updateId chatId update):rest) (HandlerData botMsgs lastUpdateId d) = do
  settings <- ask
  let (isAskedForRepetitions, userRepetitions) =
        Map.findWithDefault (False, repetitionsNum settings) chatId d
  let usersData =
        if Map.member chatId d
          then d
          else Map.insert chatId (False, repetitionsNum settings) d
  case (update, isAskedForRepetitions) of
    (TextMsg textMsg _, True) -> do
      let maybeRepetitions = readRepetitions textMsg
      case maybeRepetitions of
        Nothing -> do
          logDebug $
            BS8.concat
              [ "Update id: "
              , BS8.pack $ show updateId
              , ". Incorrect answer on \"/repeat\" command."
              ]
          handleUpdates
            rest
            (HandlerData
               (( TextMsgJSON $
                  BotTextMsgJSON
                    chatId
                    Nothing
                    (incorrectAnswer settings)
                    Nothing
                , 1) :
                botMsgs)
               updateId
               usersData)
        Just n -> do
          logDebug $
            BS8.concat
              [ "Update id: "
              , BS8.pack $ show updateId
              , ". Answer on \"/repeat\"."
              ]
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
               (Map.insert chatId (False, n) d))
    (_, True) -> do
      logDebug $
        BS8.concat
          [ "Update id: "
          , BS8.pack $ show updateId
          , ". Incorrect answer on \"/repeat\" command."
          ]
      handleUpdates
        rest
        (HandlerData
           (( TextMsgJSON $
              BotTextMsgJSON chatId Nothing (incorrectAnswer settings) Nothing
            , 1) :
            botMsgs)
           updateId
           usersData)
    (TextMsg textMsg entities, _) ->
      if head textMsg == '/'
        then do
          logDebug $
            BS8.concat
              ["Update id: ", BS8.pack $ show updateId, ". Text command."]
          let answerMsg =
                case textMsg of
                  "/help" ->
                    TextMsgJSON $
                    BotTextMsgJSON chatId Nothing (helpMessage settings) Nothing
                  "/repeat" ->
                    TextMsgJSON $
                    BotTextMsgJSON
                      chatId
                      Nothing
                      (repeatMessage settings ++
                       " Now I am repeating your messages " ++
                       show userRepetitions ++ " times.")
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
                      (Map.insert chatId (True, userRepetitions) usersData))
            else handleUpdates
                   rest
                   (HandlerData ((answerMsg, 1) : botMsgs) updateId usersData)
        else do
          logDebug $
            BS8.concat
              ["Update id: ", BS8.pack $ show updateId, ". Text message."]
          handleUpdates
            rest
            (HandlerData
               ((composeTextMsg chatId textMsg entities, userRepetitions) :
                botMsgs)
               updateId
               usersData)
    (StickerMsg fileId, _) -> do
      logDebug $
        BS8.concat
          ["Update id: ", BS8.pack $ show updateId, ". Sticker message."]
      handleUpdates
        rest
        (HandlerData
           ((StickerMsgJSON $ BotStickerMsgJSON chatId fileId, 1) : botMsgs)
           updateId
           usersData)
    (UnknownMsg, _) -> do
      logWarning $
        BS8.concat
          ["Update id: ", BS8.pack $ show updateId, ". Unknown message."]
      lift $ putStrLn "Unknown message."
      handleUpdates rest (HandlerData botMsgs updateId usersData)
  where
    readRepetitions t =
      case readMaybe t of
        Just n ->
          if n > 0 && n < 6
            then Just n
            else Nothing
        _ -> Nothing

composeTextMsg ::
     ChatId -> String -> Maybe [TelegramEntity] -> TelegramBotMsgJSON
composeTextMsg chatId textMsg entities =
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
