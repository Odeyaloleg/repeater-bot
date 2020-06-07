{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Tests.Telegram.ToJSON
  ( runTests
  ) where

import Data.Aeson (encode)
import Telegram.ToJSON (BotStickerMsgJSON(..))
import Test.QuickCheck (Property, quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_botStickerMsgJSON :: Property
prop_botStickerMsgJSON =
  monadicIO $ do
    let result = encode $ BotStickerMsgJSON 12345 "12345abcd"
    assert $ result == "{\"sticker\":\"12345abcd\",\"chat_id\":12345}"

return []

runTests' = $quickCheckAll

runTests :: IO ()
runTests =
  runTests' >>= \passed ->
    if passed
      then putStrLn "All Telegram encoding to JSON tests passed."
      else putStrLn "Some Telegram encoding to JSON tests failed."
