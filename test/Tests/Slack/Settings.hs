{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Tests.Slack.Settings
  ( runTests
  ) where

import qualified Data.Map.Strict as MS
import qualified Data.ByteString.Char8 as BS8
import Slack.Settings
  ( SlackSettings(..)
  , ServerSettings(..)
  , SlackTextAnswers(..)
  , setSlackSettings
  )
import Test.QuickCheck (Property, quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_correctData :: Property
prop_correctData =
  monadicIO $ do
    let result =
          setSlackSettings $
          (MS.fromList
            [ ("SlackToken", "BotToken")
            , ("ServerIP", "127.0.0.1")
            , ("ServerPort", "3000")
            , ("CommandHelp", "HelpMsg")
            , ("CommandRepeat", "RepeatMsg")
            ] :: MS.Map BS8.ByteString BS8.ByteString)
    assert $
      result ==
      Just
        (SlackSettings
           "BotToken"
           (ServerSettings "127.0.0.1" 3000)
           (SlackTextAnswers "HelpMsg" "RepeatMsg"))

return []

runTests' = $quickCheckAll

runTests :: IO ()
runTests =
  runTests' >>= \passed ->
    if passed
      then putStrLn "All Slack settings tests passed."
      else putStrLn "Some Slack settings tests failed."
