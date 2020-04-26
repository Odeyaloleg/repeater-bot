{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Tests.Telegram.Settings
  ( runTests
  ) where

import qualified Data.Map.Strict as MS
import qualified Data.ByteString.Char8 as BS8
import Telegram.Settings
  ( RequestSettings(..)
  , TelegramSettings(..)
  , setTelegramSettings
  )
import Test.QuickCheck (Property, quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_correctData :: Property
prop_correctData =
  monadicIO $ do
    let result =
          setTelegramSettings $
          (MS.fromList
            [ ("TelegramToken", "BotToken")
            , ("ProxyIP", "")
            , ("ProxyPort", "")
            , ("PollingTimeout", "60")
            , ("RepetitionsNumber", "1")
            , ("CommandHelp", "HelpMsg")
            , ("CommandRepeat", "RepeatMsg")
            ] :: MS.Map BS8.ByteString BS8.ByteString)
    assert $
      result ==
      Just
        (TelegramSettings
           (RequestSettings "BotToken" Nothing)
           60
           1
           "HelpMsg"
           "RepeatMsg")

return []

runTests' = $quickCheckAll

runTests :: IO ()
runTests =
  runTests' >>= \passed ->
    if passed
      then putStrLn "All Telegram settings tests passed."
      else putStrLn "Some Telegram settings tests failed."
