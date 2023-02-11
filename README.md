![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/gKrokod/botReborn?style=flat-square)
![Lines of code](https://img.shields.io/tokei/lines/github/gKrokod/botReborn?style=flat-square)

## What is botReborn? ##

BotReborn is a echo-bot that have to send a message from the user to 
him in response multiple times. Echo-bot work with user  throught several delivery mechanisms
specified in the configuration file `/config/bot.cfg` 
+ Console: the user's message is entered from stdin, the bot's response is sent to stdout. `telegrammode = off`
+ Telegram: https://core.telegram.org/bots/api#poll `telegrammode = on`

## Distribution ##

Place to get the latest botReborn: 
+ the git repository [GitHub](https://github.com/gKrokod/botReborn).

## Installation ##

This project uses The Haskell Tool Stack. Go check it out if you don't have it locally installed https://docs.haskellstack.org/en/stable/ .
Once you have installed The Haskell Tool stack, you need to make a configuration file `/config/bot.cfg`  (the repository has a template file for this `/config/readMe.txt`). 

<details><summary>template configuration file</summary>
    
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
    
</details>

after build this project
```
$ stack build
```

and run (e.g. Linux)
```
$ stack exec botReborn-exe
```

## Documentation ##

<details><summary>Structure of botReborn</summary> <image src="config/botReborn.svg" alt="structure"></details>

<details><summary>Idea of organizing the program</summary>

  В программе представлен объект под названием stack message в виде tuple
(Maybe Message, Maybe LastMessage), где
  
  * Maybe Message - новое необработанное сообщение.
  * Maybe LastMessage - последнее обработанное сообщение.
     
  Возможные состояния stack message:
  1. (Nothing, Nothing) - инициализируется  при запуске.
  2. (Just msg, Nothing) - при получении первого сообщения.
  3. (Nothing, Just msg) - желаемое состояние, когда программа обработала все поступившие сообщения.
  4. (Just newMsg, Just msg) - промежуточное состояние, когда программа уже обрабатывала сообщение и поступило новое.
  
   События, изменяющие состояние stack message::
  1. Инициализация при запуске программы.
  2. Поступление нового сообщения.
  3. Обработка поступившего сообщения.
  
  Цель программы: поддерживать объект stack message в состоянии (Nothing, Just msg). 
  Для этого используется 2  + n  постоянно работающих потоков, где n количество пользователей.
  
</details>

<details><summary>Description of threads</summary>
  
  1. Основной поток Main (main.hs / main, forever dispatcher)
  
    Цель: поддерживать объект stack message в состоянии (Nothing, Just msg).
    
    Задачи:
      - Загрузить параметры из configuration file.
      - Сформировать окружение для работы.
      - инициализировать объект stack message в состоянии (Nothing, Nothing).
      - запустить поток Watch.
      - запустить потоки Bot при необходимости. При получении сообщения от нового пользователя 
      (т.е. состояние stack message (Just msg, _)) сохранить его в базе данных и запустить поток Bot,
      обрабатывающий сообщения только от данного пользователя. 

  2. Watch (Handlers/Dispatcher.hs / watcherForNewMessage)
   
    Цель: состояние stack message (Just msg, _).
    
    Задачи: 
    - при обнаружении в stack message состояния (Nothing, _), т.е. нет нового необработанного сообщения, 
    запрашивать до получения новое сообщение у выбранного клиента (console, telegram).

  3. Потоки Bot (Handlers/Bot.hs / doWork)
    
    Цель: состояние stack message (Nothing, Just msg).
    
    Задачи:
    -при обнаружении состояния stack message (Just msg, _), т.е. есть новое необработанное сообщение, 
    обработать сообщение согласно заложенной логике. 
  
 
</details>

<details><summary>Main parameters of the configuration file</summary>
  
  1. repeatcount
    
    default number of repeats for user x: 1 <= x <= 5
  
  2. helpmenu
    
    text of menu on commands "/help" and "/start"
   
  3. repeatmenu
    
    text of menu on command "/repeat"
  
  4. token
    
    identifier for Telegram client
  
  5. telegrammode
    
    selection key of client version ("off" - Console client, "on" - TM client)
  
  6. lvlLog
    
    minimum log message level to display ("Debug" < "Warning" < "Error" < "Fatal")   

</details>


## Copying ##

botReborn is Charityware.  You can use and copy it as much as you like.

## Main author ##

[@gKrokod](https://github.com/gKrokod) or @ofspb (telegram)
