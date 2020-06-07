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
execTelegramBot s = runReaderT (runTelegramBot M.empty 0) s

runTelegramBot ::
     UsersData Int -> LastUpdateId -> ReaderT TelegramSettings IO ()
runTelegramBot usersData lastUpdId = do
  pollingResult <- pollTelegram lastUpdId
  case pollingResult of
    BadRequest description -> do
      lift $ putStrLn $ "Telegram polling error: " ++ description
      logRelease $
        "Telegram polling error: " `BSL8.append` BSL8.pack description
    TelegramUpdates updates -> do
      (HandlerData botMsgs lastUpdateId newUsersData) <-
        handleUpdates updates (HandlerData [] 0 usersData)
      if length botMsgs == 0
        then logDebug "There is no handled updates."
        else do
          answers <- sendMessagesNTimes botMsgs
          let failedSize = length $ filter (== AnswerFail) answers
          let succeedSize = length $ filter (== AnswerSuccess) answers
          if failedSize > 0
            then logRelease $
                 BSL8.concat
                   [ "Failed to send "
                   , (BSL8.pack $ show failedSize)
                   , "/"
                   , (BSL8.pack $ show succeedSize)
                   , " messages."
                   ]
            else logDebug $
                 BSL8.concat
                   [ "Successfully sent all "
                   , (BSL8.pack $ show succeedSize)
                   , " messages."
                   ]
      runTelegramBot newUsersData lastUpdateId
