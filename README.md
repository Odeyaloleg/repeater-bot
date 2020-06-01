# repeater-bot

### Usage:

Repeating: text messages, text links, stickers.

Available commands:
* /help(Telegram) /about(Slack) - Information about the bot.
* /repeat - Allows to choose how many times(1-5) bot will repeat messages.

Config template "bot.config" is located in "~/templates/". Make sure you copied it into your directory with executable and put your settings.

### Structure:

Module Main reads bot.config and runs bot for messenger according to config. If concrete messenger's bot has specified task or required own structure for this task, it has individual module in it's own directory. For example, Telegram has own structure for sending messages, so code for this is in module Telegram.SendingMessages.

Loop of every bot:
1. Getting message from messenger.
2. Parsing it into Haskell type.
3. Handling.
4. Encoding answer message into JSON.
5. Sending message.
