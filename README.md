![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/gKrokod/botReborn?style=flat-square)
![Lines of code](https://img.shields.io/tokei/lines/github/gKrokod/botReborn?style=flat-square)

## What is botReborn? ##

BotReborn is a echo-bot that have to send a message from the user to 
him in response multiple times. Echo-bot work with user  trhought several delivery mechanisms
specified in the configuration file `/config/bot.cfg` 
+ Console: the user's message is entered from stdin, the bot's response is sent to stdout. `telegrammode = off`
+ Telegram: https://core.telegram.org/bots/api#poll `telegrammode = on`

## Distribution ##

Place to get the latest botReborn: 
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

<details>
<summary>Основная идея организации программы</summary>
  В программе есть объект под названием stack message base, представленный в виде tuple (Maybe Message, Maybe LastMessage), где
  
  * Maybe Message - новое необработанное сообщение.
  * Maybe LastMessage - последнее обработанное сообщение.
     
  Возможные состояния stack message base:
  1. (Nothing, Nothing) - при запуске.
  2. (Just msg, Nothing) - при получении первого сообщения.
  3. (Nothing, Just msg) - желаемое состояние, когда программа обработала все поступившие сообщения.
  4. (Just newMsg, Just msg) - промежуточное состояние, когда программа уже обрабатывала сообщение и поступило новое.
  
  Цель программы: поддерживать stack message base в состоянии 3. 
  
  Для работы с stack message base применяются 3 (Main, Watch, Dispatcher) + N (Bot) потоков, где N количество пользователей.
</details>

<details>

  <summary>Описание потоков</summary>
  
  1. Main (main.hs / main)
     
    задачей является ...
      - Считывает настройки из configuration file.
      - Формирует окружение для работы и handles.
      - запускает поток Watch.
      - запускает поток Dispatcher.
  2. Watch (Handlers/Dispatcher.hs / watcherForNewMessage)
    
    - Переводит stack message base из состояния (Nothing, _) в состояние (Just msg, _) для чего пользуется выбранным
  клиентом (console, telegram).  
  3. Dispatcher (Handlers/Dispatcher.hs / dispatcher)
    
    задачей является ...
    - Намерен перевести stack message base из состояния (Just msg, _) в состояние (Nothing, Just msg) для чего рассматривает
  поступившее сообщение и решает создавать для него новый Bot поток или же просто подождать, когда ранее созданный обработает сообщение
  и изменит состояние stack message base.
  4. Bot (Handlers/Bot.hs / doWork)
  
    задачей является ...
    - Переводит stack message base из состояния (Just msg, _) в состояние (Nothing, Just msg) для чего обрабатывает
  поступившее сообщение (msg) согласно заложенной логике. Каждый Bot поток обрабатывает сообщение только от пользователя
  для которого он был запущен.
</details>

<details>
<summary>Cхематичное изображение взаимодействия потоков в программе</summary>
 
[![concept][1]][1]
 
[1]: config/5.jpg
 
</details>

## Copying ##

botReborn is Charityware.  You can use and copy it as much as you like.

## Main author ##

[@gKrokod](https://github.com/gKrokod)
