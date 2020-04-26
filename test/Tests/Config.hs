{-# LANGUAGE TemplateHaskell #-}

module Tests.Config
  ( runTests
  ) where

import Config (readConfig)
import Data.ByteString.Char8 (pack)
import qualified Data.Map.Strict as MS
import Test.QuickCheck (Property, quickCheckAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_emptyConfigSafety :: Property
prop_emptyConfigSafety =
  monadicIO $ do
    result <- run $ readConfig $ pack ""
    assert $ result == Right MS.empty

prop_badContents :: Property
prop_badContents =
  monadicIO $ do
    result <- run $ readConfig $ pack "key1=val1\n\newf,l\nqwdfwef\n\nkey2=val2"
    assert $ result == Left "parse error on line 3"

return []

runTests' = $quickCheckAll

runTests :: IO ()
runTests =
  runTests' >>= \passed ->
    if passed
      then putStrLn "All config tests passed."
      else putStrLn "Some config tests failed."
