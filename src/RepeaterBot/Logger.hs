{-# LANGUAGE OverloadedStrings #-}

module RepeaterBot.Logger where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Lazy as BSL

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
