{-# LANGUAGE FlexibleInstances #-}

module Config
  ( readConfig
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as BS8
  ( ByteString
  , append
  , head
  , lines
  , null
  , pack
  , readFile
  , tail
  )
import Data.Map.Strict (Map, fromList)
import System.IO (FilePath)

class Config a where
  readConfig :: a -> IO (Either String (Map BS8.ByteString BS8.ByteString))

instance Config FilePath where
  readConfig configName = do
    result <-
      try $ BS8.readFile configName :: IO (Either SomeException BS8.ByteString)
    case result of
      Left e -> return $ Left $ show e
      Right contents -> return $ Right $ parseConfig contents

-- For module Tests.Config
instance Config BS8.ByteString where
  readConfig = return . Right . parseConfig

parseConfig :: BS8.ByteString -> Map BS8.ByteString BS8.ByteString
parseConfig contents =
  let dataLines =
        filter
          (\line ->
             if BS8.null line || BS8.head line == '#'
               then False
               else True)
          (BS8.lines contents)
      settingsLines = fmap (tuplesOn '=') $ dataLines
   in fromList settingsLines
  where
    tuplesOn c settingData = helper c (BS8.pack "", settingData)
    helper c (fieldName, rest)
      | BS8.head rest == c = (fieldName, BS8.tail rest)
      | otherwise =
        helper
          c
          ( fieldName `BS8.append` BS8.pack ((BS8.head rest) : [])
          , BS8.tail rest)
