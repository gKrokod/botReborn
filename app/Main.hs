module Main (main) where
import Control.Monad
import Control.Concurrent
import Types
import qualified Handlers.Bot
-- import qualified ConsoleBot
import qualified Handlers.Base
import qualified Base
import qualified Handlers.Client
import qualified ClientConsole
import qualified Handlers.Dispatcher
import qualified Dispatcher
import qualified Config (loadConfig)
import System.IO
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO


main :: IO ()
main = do
  hGetBuffering stdin >>= print 
  hGetBuffering stdout >>= print
  hSetBuffering stdin LineBuffering -- чтобы логи нормально выводились с потоками.
  hSetBuffering stdout LineBuffering
  -- hGetBuffering stdin >>= print 
  -- hGetBuffering stdout >>= print
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

  let botHandle = Handlers.Bot.Handle 
               { Handlers.Bot.getMessage = undefined --Handlers.Dispatcher.getMessage dispatcherHandle 1
	       , Handlers.Bot.sendMessage = ClientConsole.carryAway
	       , Handlers.Bot.base = baseHandle
	       , Handlers.Bot.helpMessage = cTextMenuHelp cfg
	       , Handlers.Bot.repeatMessage = cTextMenuRepeat cfg}

  let handle = Handlers.Dispatcher.Handle
                   { Handlers.Dispatcher.client = clientHandle
		   , Handlers.Dispatcher.bot = botHandle
		   , Handlers.Dispatcher.forkForUser = Dispatcher.forkForUser
		   }
  -- hSetBuffering stdout NoBuffering
  forkIO $ forever ( do  
    Handlers.Dispatcher.watcherForNewMessage handle
    threadDelay (100))
  print "nice"
  forever $ reaction handle 
--работает, но не выходит чего-то из ghci, при запуске exe все норм
reaction :: Handlers.Dispatcher.Handle IO -> IO ()
reaction h = do
  (stack, lastMsg) <- Handlers.Base.readStackMessage (Handlers.Bot.base $ Handlers.Dispatcher.bot h)
  case stack of
    Nothing -> reaction h
    Just msg -> do
      let h' = Handlers.Dispatcher.bot h
      let user = mUser msg
      -- when (mData msg == Msg _) (error "exita ne bydet")
      existUser <- Handlers.Base.findUser (Handlers.Bot.base h') (user) 
      case existUser of
          Just _ -> pure ()
	  Nothing -> do
            Handlers.Base.updateUser (Handlers.Bot.base h') (user) (Handlers.Base.defaultRepeatCount (Handlers.Bot.base h'))
            forkIO $ do
              print ("Make FORK for user: " <> show user)
	      forever $ Handlers.Bot.doWork (h' {Handlers.Bot.getMessage = Handlers.Dispatcher.getMessage h user})
            print ("potok for user " <> show user)


-- giveRepeatCountFromBase :: (Monad m) => Handle m -> User -> m (RepeatCount)
-- giveRepeatCountFromBase h user = do
--   existUser <- findUser h user
--   case existUser of
--     Nothing -> do
--       updateUser h user (defaultRepeatCount h)
--       giveRepeatCountFromBase h user
--     Just repeatCount -> pure repeatCount
-- данный вариант тоже работает
--   forkIO $ forever ( do  
--     Handlers.Dispatcher.watcherForNewMessage handle
--     threadDelay (100))
--   print "nice"
--   forever $ reaction handle 
-- reaction :: Handlers.Dispatcher.Handle IO -> IO ()
-- reaction h = do
--   (stack, lastMsg) <- Handlers.Base.readStackMessage (Handlers.Bot.base $ Handlers.Dispatcher.bot h)
--   case stack of
--     Nothing -> reaction h
--     Just msg -> do
--       let h' = Handlers.Dispatcher.bot h
--       let user = mUser msg
--       msg <- Handlers.Bot.getMessage (h' {Handlers.Bot.getMessage = Handlers.Dispatcher.getMessage h user})
--       print $ mID msg
--       Handlers.Bot.makeReaction (h' {Handlers.Bot.getMessage = Handlers.Dispatcher.getMessage h user}) msg
--
-- вот этот код работает со старым вотчером
		   
--   let handlebot = botHandle {Handlers.Bot.getMessage = Handlers.Dispatcher.getMessage handle 1} 
-- -- Запускаем смотрителя за новыми сообщениями в отдельном потоке
--   forkIO $ forever $ Handlers.Dispatcher.watcherForNewMessage handle
-- -- ЗАпускаем считывание сообщений для пользователя один и ответы на них, тут бы потоки создавать конечно
-- -- ну и бардак с хендлирами у меня
--   forever $ do
--     Handlers.Bot.doWork handlebot
--
-- loop :: Handlers.Bot.Handle IO -> IO ()
-- loop h = do
--   msg <- Handlers.Bot.getMessage h
--   print $ mID msg
--   Handlers.Bot.makeReaction h msg
--   loop h
