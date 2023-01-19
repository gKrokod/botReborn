module Handlers.Base where
import Types (RepeatCount, User, Message, LastMessage)

data Handle m = Handle 
  {  defaultRepeatCount :: RepeatCount
  ,  readStackMessage :: m (Maybe Message, Maybe LastMessage)
  ,  saveMessage :: Message -> m ()
  ,  eraseMessage :: Message -> m ()
  ,  findUser :: User -> m (Maybe RepeatCount)
  ,  updateUser :: User -> RepeatCount -> m ()
  }


giveRepeatCountFromBase :: (Monad m) => Handle m -> User -> m (RepeatCount)
giveRepeatCountFromBase h user = do
  existUser <- findUser h user
  case existUser of
    Nothing -> do
      updateUser h user (defaultRepeatCount h)
      giveRepeatCountFromBase h user
    Just repeatCount -> pure repeatCount

