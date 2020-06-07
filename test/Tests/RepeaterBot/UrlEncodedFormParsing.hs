{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Tests.RepeaterBot.UrlEncodedFormParsing
  ( runTests
  ) where

import RepeaterBot.UrlEncodedFormParsing (getVal, parseUrlEncoded)
import Test.QuickCheck (Property, quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_simpleTest :: Property
prop_simpleTest =
  monadicIO $ do
    let prasedData = parseUrlEncoded "key1=val1&key2=val2"
    assert $ maybe False (== "val2") (getVal "key2" prasedData)

prop_oddAmpresand :: Property
prop_oddAmpresand =
  monadicIO $ do
    let prasedData = parseUrlEncoded "key1=val1&&key2=val2&key3=val3&"
    assert $ maybe False (== "val3") (getVal "key3" prasedData)

return []

runTests' = $quickCheckAll

runTests :: IO ()
runTests =
  runTests' >>= \passed ->
    if passed
      then putStrLn "All UrlEncoded tests passed."
      else putStrLn "Some UrlEncoded tests failed."
