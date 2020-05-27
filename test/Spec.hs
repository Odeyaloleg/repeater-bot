import qualified Tests.Config
import qualified Tests.Slack.Parsing
import qualified Tests.Slack.Settings
import qualified Tests.Slack.ToJSON
import qualified Tests.Telegram.Parsing
import qualified Tests.Telegram.Settings
import qualified Tests.Telegram.ToJSON
import qualified Tests.UrlEncodedFormParsing

main :: IO ()
main = do
  Tests.Config.runTests
  Tests.UrlEncodedFormParsing.runTests
  Tests.Telegram.Parsing.runTests
  Tests.Telegram.Settings.runTests
  Tests.Telegram.ToJSON.runTests
  Tests.Slack.Parsing.runTests
  Tests.Slack.Settings.runTests
  Tests.Slack.ToJSON.runTests
