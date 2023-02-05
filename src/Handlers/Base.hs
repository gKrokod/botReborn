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
  Handlers.Logger.logMessage logHandle Debug
    "ищем пользователя в базе"
  existUser <- findUser h user
  case existUser of
    Nothing -> do
      Handlers.Logger.logMessage logHandle Debug
        "пользователь в базе не найден"
      Handlers.Logger.logMessage logHandle Debug
        "сохраняем пользователя в базе"
      updateUser h user (defaultRepeatCount h)
      Handlers.Logger.logMessage logHandle Debug
        "считываем количество повторов для пользователя из базы"
      giveRepeatCountFromBase h user
    Just repeatCount -> do
      Handlers.Logger.logMessage logHandle Debug
        "считываем количество повторов для пользователя из базы"
      pure repeatCount

