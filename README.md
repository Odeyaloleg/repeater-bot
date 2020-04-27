# repeater-bot

### Usage:

Repeating: text messages, text links, stickers.

Available commands:
* /help(Telegram) /about(Slack) - Information about the bot.
* /repeat - Allows to choose how many times(1-5) bot will repeat messages.

Configure bot in ~/bot.config.

### Structure:

Module Main reads bot.config and runs bots of every messanger. If concrete messanger's bot has specified task or required own structure for this task, it has individual module in it's own directory. For example, Telegram has own structure for sending messages, so code for this is in module Telegram.SendingMessages.

Loop of every bot:
1. Getting message from messanger.
2. Parsing it into Haskell type.
3. Handling.
4. Encoding own message into JSON.
5. Sending message.
