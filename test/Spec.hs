import qualified Tests.RepeaterBot.Config
import qualified Tests.RepeaterBot.UrlEncodedFormParsing
import qualified Tests.Slack.Parsing
import qualified Tests.Slack.Settings
import qualified Tests.Slack.ToJSON
import qualified Tests.Telegram.Parsing
import qualified Tests.Telegram.Settings
import qualified Tests.Telegram.ToJSON

main :: IO ()
main = do
  Tests.RepeaterBot.Config.runTests
  Tests.RepeaterBot.UrlEncodedFormParsing.runTests
  Tests.Telegram.Parsing.runTests
  Tests.Telegram.Settings.runTests
  Tests.Telegram.ToJSON.runTests
  Tests.Slack.Parsing.runTests
  Tests.Slack.Settings.runTests
  Tests.Slack.ToJSON.runTests
