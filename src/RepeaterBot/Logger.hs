{-# LANGUAGE OverloadedStrings #-}

module RepeaterBot.Logger where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

data LogLevel
  = LevelDEBUG
  | LevelWARN
  | LevelRELEASE
  deriving (Eq, Ord)

class Logger a where
  logDebug :: BSL.ByteString -> ReaderT a IO ()
  logWarning :: BSL.ByteString -> ReaderT a IO ()
  logRelease :: BSL.ByteString -> ReaderT a IO ()

writeLogIn :: FilePath -> BSL.ByteString -> IO ()
writeLogIn fileName log = do
  result <-
    try $ BSL.appendFile fileName (log `BSL.append` "\n") :: IO (Either SomeException ())
  case result of
    Left e -> putStrLn $ "Logger error: " ++ show e
    Right _ -> return ()

type SucceedAnswersSize = Int

type FailedAnswersSize = Int

logSendingResult ::
     (Logger a) => SucceedAnswersSize -> FailedAnswersSize -> ReaderT a IO ()
logSendingResult succeedSize failedSize =
  if failedSize > 0
    then logRelease $
         BSL8.concat
           [ "Failed to send "
           , BSL8.pack $ show failedSize
           , "/"
           , BSL8.pack $ show (succeedSize + failedSize)
           , " messages."
           ]
    else logDebug $
         BSL8.concat
           [ "Successfully sent all "
           , BSL8.pack $ show succeedSize
           , " messages."
           ]
