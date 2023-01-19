module Main (main) where
import Control.Monad
import Control.Concurrent
import Types
import qualified Handlers.Bot
import qualified ConsoleBot
import qualified Handlers.Base
import qualified Base
import qualified Handlers.Client
import qualified ClientConsole
import qualified Handlers.Dispatcher
import qualified Config (loadConfig)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


-- data Handle m = Handle
--   { client :: Handlers.Client.Handle m
--   , base :: Handlers.Base.Handle m
--   }

-- data Handle m = Handle
--   { fetch :: Maybe LastMessage -> m (Maybe Message)
--   , carryAway :: Message -> m ()
--   }
--
main :: IO ()
main = do
  -- load config and make handles
  stackMessage <- Base.newBaseMessage
  base <- Base.newBaseUser
  cfg <- Config.loadConfig

  let baseHandle = Handlers.Base.Handle
                   {  Handlers.Base.defaultRepeatCount = cRepeatCount cfg
		   ,  Handlers.Base.readStackMessage = Base.readStackMessage stackMessage
		   ,  Handlers.Base.saveMessage = Base.saveMessage stackMessage
		   ,  Handlers.Base.eraseMessage = Base.eraseMessage stackMessage
		   ,  Handlers.Base.findUser = Base.findUser base
		   ,  Handlers.Base.updateUser = Base.updateUser base
		   }
  
  let clientHandle = Handlers.Client.Handle
                     { Handlers.Client.fetch = ClientConsole.fetch
		     , Handlers.Client.carryAway = ClientConsole.carryAway
	             }


  let dispatcherHandle = Handlers.Dispatcher.Handle
                   { Handlers.Dispatcher.client = clientHandle
		   , Handlers.Dispatcher.base = baseHandle
		   }

  let handle = Handlers.Bot.Handle 
               { Handlers.Bot.getMessage = Handlers.Dispatcher.getMessage dispatcherHandle 1
	       , Handlers.Bot.sendMessage = ClientConsole.carryAway
	       , Handlers.Bot.base = baseHandle
	       , Handlers.Bot.helpMessage = cTextMenuHelp cfg
	       , Handlers.Bot.repeatMessage = cTextMenuRepeat cfg}
-- Запускаем смотрителя за новыми сообщениями в отдельном потоке
  forkIO $ forever $ Handlers.Dispatcher.watcherForNewMessage dispatcherHandle
-- ЗАпускаем считывание сообщений для пользователя один и ответы на них, тут бы потоки создавать конечно
-- ну и бардак с хендлирами у меня
  forever $ do
   -- Handlers.Dispatcher.watcherForNewMessage dispatcherHandle
   msg <- Handlers.Bot.getMessage handle
   print $ mID msg
   Handlers.Bot.makeReaction handle msg

  -- do logic
  -- loop handle

loop :: Handlers.Bot.Handle IO -> IO ()
loop h = do
  msg <- Handlers.Bot.getMessage h
  print $ mID msg
  Handlers.Bot.makeReaction h msg
  loop h
