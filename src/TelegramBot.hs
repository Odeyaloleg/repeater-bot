{-# LANGUAGE OverloadedStrings #-}

module TelegramBot
  ( execTelegramBot
  ) where

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import qualified Data.Map as M
import Telegram.BotModel (LastUpdateId)
import Telegram.HandleUpdates (HandlerData(..), handleUpdates)
import Telegram.Parsing (AnswerStatus(..), TelegramUpdates(..))
import Telegram.Polling (pollTelegram)
import Telegram.SendingMessages (sendMessagesNTimes)
import Telegram.Settings (TelegramSettings(..))
import UsersData (UsersData)

execTelegramBot :: TelegramSettings -> IO ()
execTelegramBot s = runReaderT (runTelegramBot M.empty 0) s

runTelegramBot ::
     UsersData Int -> LastUpdateId -> ReaderT TelegramSettings IO ()
runTelegramBot usersData lastUpdId = do
  pollingResult <- pollTelegram lastUpdId
  case pollingResult of
    BadRequest description ->
      lift $ putStrLn $ "Telegram polling error: " ++ description
    TelegramUpdates updates -> do
      (HandlerData botMsgs lastUpdateId newUsersData) <-
        handleUpdates updates (HandlerData [] 0 usersData)
      if length botMsgs == 0
        then lift $
             putStrLn $
             "Telegram: Didn't get any messages possible for handling."
        else do
          settings <- ask
          succeedAnswers <-
            lift $
            sendMessagesNTimes
              botMsgs
              (requestSettings settings)
              (logLevel settings)
          lift $
            putStrLn $
            "Telegram: Handled " ++
            show (length botMsgs) ++
            "/" ++
            show (length updates) ++
            " messages. Sent " ++
            show (length (filter (== AnswerSuccess) succeedAnswers)) ++
            "/" ++ show (length succeedAnswers) ++ " messages."
      runTelegramBot newUsersData lastUpdateId
