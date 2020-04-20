module Tests.Config
  ( runTests
  ) where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Config
import qualified Data.Map.Strict as MS
import Data.ByteString.Char8 (pack)

runTests :: IO ()
runTests = quickCheck prop_emptyConfigSafety

prop_emptyConfigSafety :: Property
prop_emptyConfigSafety = monadicIO $ do
  result <- run $ readConfig $ pack ""
  assert $ result == Right MS.empty
