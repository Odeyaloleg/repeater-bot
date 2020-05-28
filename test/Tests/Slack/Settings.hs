{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Tests.Slack.Settings
  ( runTests
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as MS
import Logger (LogLevel(..))
import Settings (setBotSettings)
import Slack.Settings
  ( ServerSettings(..)
  , SlackSettings(..)
  , SlackTextAnswers(..)
  )
import Test.QuickCheck (Property, quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_correctData :: Property
prop_correctData =
  monadicIO $ do
    let result =
          setBotSettings $
          (MS.fromList
             [ ("SlackToken", "BotToken")
             , ("ServerIP", "127.0.0.1")
             , ("ServerPort", "3000")
             , ("RepetitionsNumber", "1")
             , ("CommandHelp", "HelpMsg")
             , ("CommandRepeat", "RepeatMsg")
             , ("SlackLogLevel", "DEBUG")
             ] :: MS.Map BS8.ByteString BS8.ByteString)
    assert $
      result ==
      Just
        (SlackSettings
           "BotToken"
           (ServerSettings "127.0.0.1" 3000)
           1
           (SlackTextAnswers "HelpMsg" "RepeatMsg")
           LevelDEBUG)

return []

runTests' = $quickCheckAll

runTests :: IO ()
runTests =
  runTests' >>= \passed ->
    if passed
      then putStrLn "All Slack settings tests passed."
      else putStrLn "Some Slack settings tests failed."
