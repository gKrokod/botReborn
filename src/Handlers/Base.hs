module Handlers.Base (giveRepeatCountFromBase, Handle (..)) where

import qualified Handlers.Logger
import Types (LastMessage, Log (..), Message, RepeatCount, User)

data Handle m = Handle
  { defaultRepeatCount :: RepeatCount,
    readStackMessage :: m (Maybe Message, Maybe LastMessage),
    saveMessage :: Message -> m (),
    eraseMessage :: Message -> m (),
    findUser :: User -> m (Maybe RepeatCount),
    updateUser :: User -> RepeatCount -> m (),
    logger :: Handlers.Logger.Handle m
  }

giveRepeatCountFromBase :: (Monad m) => Handle m -> User -> m RepeatCount
giveRepeatCountFromBase h user = do
  let logHandle = logger h
  Handlers.Logger.logMessage
    logHandle
    Debug
    "Base. Looking for the user in the database"
  existUser <- findUser h user
  case existUser of
    Nothing -> do
      Handlers.Logger.logMessage
        logHandle
        Debug
        "Base. The user not found in the database"
      Handlers.Logger.logMessage
        logHandle
        Debug
        "Base. Save the user in the database"
      updateUser h user (defaultRepeatCount h)
      Handlers.Logger.logMessage
        logHandle
        Debug
        "Base. Get number of repeats for user from the database"
      giveRepeatCountFromBase h user
    Just repeatCount -> do
      Handlers.Logger.logMessage
        logHandle
        Debug
        "Base. Get number of repeats for user from the database"
      pure repeatCount
