module Handlers.Base where
import Types (RepeatCount, User, Message, LastMessage, Log (..) )
import qualified Handlers.Logger

data Handle m = Handle 
  {  defaultRepeatCount :: RepeatCount
  ,  readStackMessage :: m (Maybe Message, Maybe LastMessage)
  ,  saveMessage :: Message -> m ()
  ,  eraseMessage :: Message -> m ()
  ,  findUser :: User -> m (Maybe RepeatCount)
  ,  updateUser :: User -> RepeatCount -> m ()
  ,  logger :: Handlers.Logger.Handle m  
  }


giveRepeatCountFromBase :: (Monad m) => Handle m -> User -> m (RepeatCount)
giveRepeatCountFromBase h user = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Debug "find the user in base"
  existUser <- findUser h user
  case existUser of
    Nothing -> do
      Handlers.Logger.logMessage logHandle Debug "didn't find the user in base"
      Handlers.Logger.logMessage logHandle Debug "save the user in base"
      updateUser h user (defaultRepeatCount h)
      Handlers.Logger.logMessage logHandle Debug "read repeat count for user from base"
      giveRepeatCountFromBase h user
    Just repeatCount -> do
      Handlers.Logger.logMessage logHandle Debug "read repeat count for user from base"
      pure repeatCount

