![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/gKrokod/botReborn?style=flat-square)
![Lines of code](https://img.shields.io/tokei/lines/github/gKrokod/botReborn?style=flat-square)

## What is botReborn? ##

BotReborn is a echo-bot that send multiple text or gif messages to еach message from the user.
Echo-bot interacts with user through some delivery mechanisms specified in the configuration file `/config/bot.cfg` 
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
  
  There is an object called "stack message" in the form of tuple data types
  (Maybe Message, Maybe LastMessage), where
  
  * Maybe Message - new incoming message.
  * Maybe LastMessage - last outcoming message.

  Possible stack message states:
  1. (Nothing, Nothing) - initialization at program start.
  2. (Just msg, Nothing) - receiving the first message.
  3. (Nothing, Just msg) - the desired state, when the program has processed all incoming messages.
  4. (Just newMsg, Just msg) - an intermediate state, when the program has already processed the message and a new one has arrived.
  
  Events that change the state of the stack message:
  1. Initialization at program start.
  2. New incoming message.
  3. Processing the message.
  
  The goal of the program: to keep the stack message object in the state (Nothing, Just msg).
  
  There are 2 + n constantly running threads for this goal, where n is the number of users.
  
</details>

<details><summary>Description of threads</summary>
  
  1. Main thread (main.hs / main, forever dispatcher)
  
    The goal: to keep the stack message object in the state (Nothing, Just msg).
    
    Tasks:
      - Load parameters from configuration file.
      - Create an environment for work.
      - initialize the stack message object in the state (Nothing, Nothing).
      - run the Watch thread.
      - run Bot threads if necessary. Run the Bot thread processing messages only 
      from the one user for each user in the database. Store a user in the database when
      first receiving a message from him.
    
  2. Watch thread (Handlers/Dispatcher.hs / watcherForNewMessage)
   
    The goal: stack message state (Just msg, _).
    
    Tasks: 
    - Regularly reuest a new message from the selected client (console, telegram) 
    when stack message state is (Nothing, _), i.e. no new incoming message.
    
  3. Bot treads (Handlers/Bot.hs / doWork)
    
    The goal: stack message state (Nothing, Just msg).
    
    Tasks:
    - Process the message according to the underlying logic
    when stack message state is (Just msg, _), i.e. there is new incoming message.

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
    
    selection key of client version ("off" - Console client, "on" - Telegram client)
  
  6. lvlLog
    
    minimum log message level to display ("Debug" < "Warning" < "Error" < "Fatal")   

</details>


## Copying ##

botReborn is Charityware.  You can use and copy it as much as you like.

## Main author ##

[@gKrokod](https://github.com/gKrokod) or @ofspb (telegram)
