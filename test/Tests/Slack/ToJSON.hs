{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Tests.Slack.ToJSON
  ( runTests
  ) where

import Data.Aeson (encode)
import Slack.ToJSON (SlackChallengeJSON(..))
import Test.QuickCheck (Property, quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_slackChallengeJSON :: Property
prop_slackChallengeJSON =
  monadicIO $ do
    let result = encode $ SlackChallengeJSON "12345abcd"
    assert $ result == "{\"challenge\":\"12345abcd\"}"

return []

runTests' = $quickCheckAll

runTests :: IO ()
runTests =
  runTests' >>= \passed ->
    if passed
      then putStrLn "All Slack encoding to JSON tests passed."
      else putStrLn "Some Slack encoding to JSON tests failed."
