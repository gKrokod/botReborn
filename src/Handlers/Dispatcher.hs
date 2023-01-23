module Handlers.Dispatcher where
import qualified Handlers.Base
import qualified Handlers.Client
import qualified Handlers.Bot
import Types

data Handle m = Handle
  { client :: Handlers.Client.Handle m
  , bot :: Handlers.Bot.Handle m
  , forkForUser :: m () -> m ()
  }

dispatcher :: (Monad m) => Handle m -> m ()
dispatcher h = do
  let botHandle = bot h
  let baseHandle = Handlers.Bot.base botHandle
  (stack, lastMsg) <- Handlers.Base.readStackMessage baseHandle
  case stack of
    Nothing -> pure ()
    Just msg -> do
      let user = mUser msg
      existUser <- Handlers.Base.findUser baseHandle user
      case existUser of
        Just _ -> pure ()
        Nothing -> do
	 -- если в базе пользователя нет, то сохрани его в базе с дефолтными настройкам и создай для него поток
          Handlers.Base.updateUser baseHandle user (Handlers.Base.defaultRepeatCount baseHandle)
	  forkForUser h 
	    (Handlers.Bot.doWork (botHandle {Handlers.Bot.getMessage = getMessage h user}))
	 
------------------------------------------------------------------------------------------------------------------
getMessage :: (Monad m) => Handle m -> User -> m (Message)
getMessage h user = do
  (stack, lastMsg) <- Handlers.Base.readStackMessage (Handlers.Bot.base $ bot h)
  case stack of
    Just msg -> if mUser msg == user
                then (do Handlers.Base.eraseMessage (Handlers.Bot.base $ bot h) msg; pure msg)
                else getMessage h user
    Nothing -> getMessage h user 


-- рабочий вотчер, если не создавать новых потоков
-- Если стэк с сообщениями пустой, то получает у клиента еще одно
watcherForNewMessage :: (Monad m) => Handle m -> m ()
watcherForNewMessage h = do
  let baseHandle = Handlers.Bot.base (bot h)
  let clientHandle = client h
  (stack, lastMsg) <- Handlers.Base.readStackMessage baseHandle
  case stack of
    Just _ -> pure ()
    Nothing -> loop
      where loop = do
              fetchedMessage <- Handlers.Client.fetch clientHandle lastMsg
	      case fetchedMessage of
	        Nothing -> loop
		Just msg -> Handlers.Base.saveMessage baseHandle msg


