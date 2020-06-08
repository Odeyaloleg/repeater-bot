{-# LANGUAGE OverloadedStrings #-}

module Telegram.Polling where

import Control.Monad.Reader (ReaderT, ask, lift)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client.Internal
  ( ResponseTimeout(ResponseTimeoutMicro)
  , parseRequest
  , proxy
  , responseTimeout
  )
import Network.HTTP.Simple (getResponseBody, httpLBS)
import RepeaterBot.Logger (logDebug, logWarning)
import Telegram.BotModel (LastUpdateId, botUri)
import Telegram.Parsing (TelegramUpdates(..))
import Telegram.Settings (RequestSettings(..), TelegramSettings(..))

pollTelegram :: LastUpdateId -> ReaderT TelegramSettings IO TelegramUpdates
pollTelegram lastUpdId = do
  settings <- ask
  let offset = lastUpdId + 1
  request <-
    parseRequest $
    botUri ++
    botToken (requestSettings settings) ++
    "/getUpdates?timeout=" ++
    show (pollingTimeout settings) ++ "&offset=" ++ show offset
  logDebug "Polling Telegram."
  response <-
    httpLBS $
    request
      { responseTimeout =
          ResponseTimeoutMicro (pollingTimeout settings * 1000000 + 10000000)
      , proxy = proxyServer (requestSettings settings)
      }
  let responseBody = getResponseBody response :: BSL.ByteString
  logDebug $ "Telegram response: " `BSL.append` responseBody
  case (decode responseBody :: Maybe TelegramUpdates) of
    Nothing -> do
      lift $ putStrLn "Couldn't parse updates."
      logWarning "Couldn't parse updates."
      return $ TelegramUpdates []
    Just telegramUpdates -> return telegramUpdates
