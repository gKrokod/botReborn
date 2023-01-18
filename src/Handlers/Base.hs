module Handlers.Base where
import Types (RepeatCount, User)

data Handle m = Handle 
  {  defaultRepeatCount :: RepeatCount
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

