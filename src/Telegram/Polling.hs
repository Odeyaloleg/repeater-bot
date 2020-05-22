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
import Telegram.Parsing (TelegramUpdates(..))
import Telegram.Settings (TelegramSettings(..), RequestSettings(..))
import Telegram.BotModel (LastUpdateId, botUri)

pollTelegram :: LastUpdateId -> ReaderT TelegramSettings IO TelegramUpdates
pollTelegram lastUpdId = do
  settings <- ask
  let offset = lastUpdId + 1
  request <-
    parseRequest $
    botUri ++
    (botToken (requestSettings settings)) ++
    "/getUpdates?timeout=" ++
    show (pollingTimeout settings) ++ "&offset=" ++ show offset
  response <-
    httpLBS $
    request
      { responseTimeout = ResponseTimeoutMicro (pollingTimeout settings * 1000 + 10000)
      , proxy = proxyServer (requestSettings settings)
      }
  let responseBody = getResponseBody response :: BSL.ByteString
  case (decode responseBody :: Maybe TelegramUpdates) of
    Nothing -> do
      lift $ putStrLn "Couldn't parse updates."
      return $ TelegramUpdates []
    Just telegramUpdates -> do
      return telegramUpdates
