module Tests.RepeaterBot.Config
  ( runTests
  ) where

import Data.ByteString.Char8 (pack)
import qualified Data.Map.Strict as MS
import RepeaterBot.Config (parseConfig)

prop_emptyConfigSafety :: Bool
prop_emptyConfigSafety =
  let result = parseConfig $ pack ""
   in result == Right MS.empty

prop_badContents :: Bool
prop_badContents =
  let result = parseConfig $ pack "key1=val1\n\newf,l\nqwdfwef\n\nkey2=val2"
   in result == Left "parse error on line 3"

runTests :: IO ()
runTests =
  if and [prop_badContents, prop_emptyConfigSafety]
    then putStrLn "All config tests passed."
    else putStrLn "Some config tests failed."
