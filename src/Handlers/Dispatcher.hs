module Handlers.Dispatcher where
import qualified Handlers.Base
import qualified Handlers.Client
import qualified Handlers.Bot
import Types
import qualified Handlers.Logger as HL 
import qualified Data.Text as T

data Handle m = Handle
  { client :: Handlers.Client.Handle m
  , bot :: Handlers.Bot.Handle m
  , logger :: HL.Handle m  
  , forkForUser :: m () -> m ()
  }

  -- HL.logMessage logHandle Debug "find the user in base"
dispatcher :: (Monad m) => Handle m -> m ()
dispatcher h = do
  let logHandle = logger h
  let botHandle = bot h
  let baseHandle = Handlers.Bot.base botHandle
  (stack, lastMsg) <- Handlers.Base.readStackMessage baseHandle
  -- HL.logMessage logHandle Debug "ReadStackMessage from Dispatcher"
  case stack of
    Nothing -> do 
      -- HL.logMessage logHandle Debug "Нет новых сообщений для обработки"
      pure ()
    Just msg -> do
      let user = mUser msg
      existUser <- Handlers.Base.findUser baseHandle user
      case existUser of
        Just _ -> pure ()
        Nothing -> do
	 -- если в базе пользователя нет, то сохрани его в базе с дефолтными настройкам и создай для него поток
          HL.logMessage logHandle Debug (mconcat ["Пользователь ", T.pack $ show user," не найден"])
          Handlers.Base.updateUser baseHandle user (Handlers.Base.defaultRepeatCount baseHandle)
          HL.logMessage logHandle Debug (mconcat ["Пользователь ", T.pack $ show user," сохранен в базе (диспетчер)"])
	  forkForUser h 
	    (Handlers.Bot.doWork (botHandle {Handlers.Bot.getMessage = getMessage h user}))
          HL.logMessage logHandle Debug (mconcat ["Запустили новый поток для пользователя: ", T.pack $ show user])
	 
------------------------------------------------------------------------------------------------------------------
getMessage :: (Monad m) => Handle m -> User -> m (Message)
getMessage h user = do
  let logHandle = logger h
  (stack, lastMsg) <- Handlers.Base.readStackMessage (Handlers.Bot.base $ bot h)
  case stack of
    Just msg -> if mUser msg == user
                then do
		  Handlers.Base.eraseMessage (Handlers.Bot.base $ bot h) msg
                  HL.logMessage logHandle Debug (mconcat ["Получено сообщение для пользователя ", T.pack $ show user, " из базы данных"])
		  pure msg
                else getMessage h user
    Nothing -> getMessage h user 


-- рабочий вотчер, если не создавать новых потоков
-- Если стэк с сообщениями пустой, то получает у клиента еще одно
watcherForNewMessage :: (Monad m) => Handle m -> m ()
watcherForNewMessage h = do
  let logHandle = logger h
  let baseHandle = Handlers.Bot.base (bot h)
  let clientHandle = client h
  (stack, lastMsg) <- Handlers.Base.readStackMessage baseHandle
  case stack of
    Just _ -> pure ()
    Nothing -> do
      HL.logMessage logHandle Debug "Нет новых необработанных сообщений от клиента, начинаем постоянный запрос"
      loop
      where loop = do
	      fetchedMessage <- Handlers.Client.fetch clientHandle lastMsg
	      case fetchedMessage of
	        Nothing -> loop
	        Just msg -> Handlers.Base.saveMessage baseHandle msg


