{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Tests.Telegram.Parsing
  ( runTests
  ) where

import Data.Aeson (decode)
import Telegram.Parsing
import Test.QuickCheck (Property, quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_answerStatus :: Property
prop_answerStatus =
  monadicIO $ do
    let result = decode "{\"ok\":true}" :: Maybe AnswerStatus
    assert $ result == Just AnswerSuccess

return []

runTests' = $quickCheckAll

runTests :: IO ()
runTests =
  runTests' >>= \passed ->
    if passed
      then putStrLn "All Telegram parsing tests passed."
      else putStrLn "Some Telegram parsing tests failed."
