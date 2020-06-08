{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Tests.Slack.Parsing
  ( runTests
  ) where

import Data.Aeson (decode)
import Slack.Parsing (SlackMsg(..))
import Test.QuickCheck (Property, quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_challenge :: Property
prop_challenge =
  monadicIO $ do
    let result =
          decode
            "{\"type\":\"url_verification\",\"challenge\":\"some_challenge\"}" :: Maybe SlackMsg
    assert $ result == Just (SlackChallenge "some_challenge")

return []

runTests' = $quickCheckAll

runTests :: IO ()
runTests =
  runTests' >>= \passed ->
    if passed
      then putStrLn "All Slack parsing tests passed."
      else putStrLn "Some Slack parsing tests failed."
