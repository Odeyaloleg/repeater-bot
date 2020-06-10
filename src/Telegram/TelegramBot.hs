{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Telegram.TelegramBot
  ( execTelegramBot
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, asks, lift, runReaderT)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map as M
import RepeaterBot.Logger
  ( LogLevel(..)
  , Logger
  , logDebug
  , logRelease
  , logSendingResult
  , logWarning
  , writeLogIn
  )
import RepeaterBot.UsersData (UsersData)
import Telegram.BotModel (LastUpdateId)
import Telegram.HandleUpdates (HandlerData(..), handleUpdates)
import Telegram.Parsing (AnswerStatus(..), TelegramUpdates(..))
import Telegram.Polling (pollTelegram)
import Telegram.SendingMessages (sendMessagesNTimes)
import Telegram.Settings (TelegramSettings(..))

execTelegramBot :: TelegramSettings -> IO ()
execTelegramBot = runReaderT (runTelegramBot M.empty 0)

runTelegramBot ::
     UsersData Int -> LastUpdateId -> ReaderT TelegramSettings IO ()
runTelegramBot usersData lastUpdId = do
  pollingResult <- pollTelegram lastUpdId
  handlePollingResult pollingResult usersData

handlePollingResult :: TelegramUpdates -> UsersData Int -> ReaderT TelegramSettings IO ()
handlePollingResult (BadRequest description) _ = do
  lift $ putStrLn $ "Telegram polling error: " ++ description
  logRelease $ "Telegram polling error: " `BSL8.append` BSL8.pack description
handlePollingResult (TelegramUpdates updates) usersData = do
  (HandlerData botMsgs lastUpdateId newUsersData) <-
    handleUpdates updates (HandlerData [] 0 usersData)
  if null botMsgs
    then logDebug "There is no handled updates."
    else do
      answers <- sendMessagesNTimes botMsgs
      let failedSize = length $ filter (== AnswerFail) answers
      let succeedSize = length $ filter (== AnswerSuccess) answers
      logSendingResult failedSize succeedSize
  runTelegramBot newUsersData lastUpdateId
