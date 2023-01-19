module Handlers.Dispatcher where
import qualified Handlers.Base
import qualified Handlers.Client
import Types

data Handle m = Handle
  { client :: Handlers.Client.Handle m
  , base :: Handlers.Base.Handle m
  }


getMessage :: (Monad m) => Handle m -> User -> m (Message)
getMessage h user = do
  (stack, lastMsg) <- Handlers.Base.readStackMessage (base h)
  case stack of
    Just msg -> if mUser msg == user
                then (do Handlers.Base.eraseMessage (base h) msg; pure msg)
                else getMessage h user
    Nothing -> getMessage h user 

makeForkForUser :: (Monad m) => Handle m -> m (Message)
makeForkForUser = undefined
--
-- Если стэк с сообщениями пустой, то получает у клиента еще одно
watcherForNewMessage :: (Monad m) => Handle m -> m ()
watcherForNewMessage h = do
  (stack, lastMsg) <- Handlers.Base.readStackMessage (base h)
  case stack of
    Just _ -> pure ()
    Nothing -> loop
      where loop = do
              fetchedMessage <- Handlers.Client.fetch (client h) lastMsg
	      case fetchedMessage of
	        Nothing -> loop
		Just msg -> Handlers.Base.saveMessage (base h) msg
