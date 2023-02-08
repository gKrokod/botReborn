![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/gKrokod/botReborn?style=flat-square)
![Lines of code](https://img.shields.io/tokei/lines/github/gKrokod/botReborn?style=flat-square)

## What is botReborn? ##

BotReborn is a echo-bot that have to send a message from the user to 
him in response multiple times. Echo-bot work with user  trhought several delivery mechanisms
specified in the configuration file `/config/bot.cfg` 
+ Console: the user's message is entered from stdin, the bot's response is sent to stdout. `telegrammode = off`
+ Telegram: https://core.telegram.org/bots/api#poll `telegrammode = on`

## Distribution ##

Place to get the latest borReborn: 
+ the git repository [GitHub](https://github.com/gKrokod/botReborn).

## Installation ##

This project uses The Haskell Tool Stack. Go check it out if you don't have it locally installed https://docs.haskellstack.org/en/stable/ .
Once you have installed The Haskell Tool stack, you need to make a configuration file `/config/bot.cfg`  (the repository has a template file for this `/config/readMe.txt`). 

```
config {
  user {
  repeatcount = "3"
  helpmenu = "Hello! I am echo-bot.\nPossible command : /help, /repeat\nWhat about me? Good to     meet you!"
  repeatmenu = "Number of repeats = "
  }
  url {
    apipath = "/bot"
    bothost = "api.telegram.org"
    timeout = "10"
    offset = "-1"
    token = "_"
    port = "443"
    method = "GET"
    secure = on
  }-
  telegrammode = off
  lvlLog = "Debug"
}
```
after build this project
```
$ stack build
```

and run (i.g. Linux)
```
$ stack exec botReborn-exe
```

## Documentation ##

sha bse bydet

<details>
<summary>Cхематичное изображение взаимодействия потоков в программе</summary>
 
[![concept][1]][1]
 
[1]: config/5.jpg
 
</details>

## Copying ##

BotReborn is Charityware.  You can use and copy it as much as you like.

## Main author ##

[@gKrokod](https://github.com/gKrokod)
