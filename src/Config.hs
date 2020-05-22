{-# LANGUAGE FlexibleInstances #-}

module Config
  ( readConfig
  , getVal
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
import Data.List (foldl')
import qualified Data.Map.Strict as MS
import System.IO (FilePath)

class Config a where
  readConfig :: a -> IO (Either String (MS.Map BS8.ByteString BS8.ByteString))

instance Config FilePath where
  readConfig configName = do
    result <-
      try $ BS8.readFile configName :: IO (Either SomeException BS8.ByteString)
    case result of
      Left e -> return $ Left $ show e
      Right contents -> return $ parseConfig contents

-- For module Tests.Config
instance Config BS8.ByteString where
  readConfig = return . parseConfig

getVal :: Ord k => k -> MS.Map k a -> Maybe a
getVal = MS.lookup

parseConfig ::
     BS8.ByteString -> Either String (MS.Map BS8.ByteString BS8.ByteString)
parseConfig contents =
  let configLines = BS8.lines contents
      settingsList = parseConfigLines configLines
   in either
        (Left . ("parse error on line " ++) . show)
        (Right . MS.fromList)
        settingsList
  where
    parseConfigLines configLines = parseLoop (1, []) configLines
    parseLoop (_, passedLines) [] = Right passedLines
    parseLoop (lineNum, passedLines) (line:rest) =
      if BS8.null line || BS8.head line == '#'
        then parseLoop (lineNum + 1, passedLines) rest
        else maybe
               (Left lineNum)
               (\setting -> parseLoop (lineNum + 1, setting : passedLines) rest)
               (tupleOn '=' line)

-- Nothing if first parameter didn't occur in the second parameter
tupleOn :: Char -> BS8.ByteString -> Maybe (BS8.ByteString, BS8.ByteString)
tupleOn c settingData = helper c (BS8.pack "", settingData)
  where
    helper c (fieldName, rest)
      | not (BS8.null rest) && BS8.head rest == c =
        Just (fieldName, BS8.tail rest)
      | otherwise =
        if BS8.null rest
          then Nothing
          else helper
                 c
                 ( fieldName `BS8.append` BS8.pack ((BS8.head rest) : [])
                 , BS8.tail rest)
