module Handlers.Dispatcher (Handle (..), dispatcher, watcherForNewMessage) where

import qualified Data.Text as T
import qualified Handlers.Base
import qualified Handlers.Bot
import qualified Handlers.Client
import qualified Handlers.Logger
import Types (Log (..), Message (..))

data Handle m = Handle
  { bot :: Handlers.Bot.Handle m,
    logger :: Handlers.Logger.Handle m,
    forkForUser :: m () -> m ()
  }

dispatcher :: (Monad m) => Handle m -> m ()
dispatcher h = do
  let logHandle = logger h
  let botHandle = bot h
  let baseHandle = Handlers.Bot.base botHandle
  -- input : message database / desired (Just msg, _)
  (mbMessage, _) <- Handlers.Base.readStackMessage baseHandle
  case mbMessage of
    Nothing -> pure () -- desired result: (Nothing, _)
    Just msg -> do
      let user = mUser msg
      existUser <- Handlers.Base.findUser baseHandle user
      case existUser of
        -- User exists. Wait. The fork Bot will get the desired result
        Just _ -> pure ()
        -- Does not. Save the user in the database and run fork Bot for him
        Nothing -> do
          Handlers.Logger.logMessage
            logHandle
            Debug
            ( mconcat
                [ "Dispatcher. The user ",
                  T.pack $ show user,
                  " not found"
                ]
            )
          Handlers.Base.updateUser baseHandle user (Handlers.Base.defaultRepeatCount baseHandle)
          Handlers.Logger.logMessage
            logHandle
            Debug
            ( mconcat
                [ "Dispatcher. The user ",
                  T.pack $ show user,
                  " save in the database"
                ]
            )
          forkForUser
            h
            (Handlers.Bot.doWork botHandle user)
          Handlers.Logger.logMessage
            logHandle
            Debug
            ( mconcat
                [ "Dispatcher. Run fork Bot for user: ",
                  T.pack $ show user
                ]
            )

watcherForNewMessage :: (Monad m) => Handle m -> m ()
watcherForNewMessage h = do
  let logHandle = logger h
  let baseHandle = Handlers.Bot.base (bot h)
  let clientHandle = Handlers.Bot.client (bot h)
  -- input : message database / desired (Nothing, _)
  (mbMessage, lastMsg) <- Handlers.Base.readStackMessage baseHandle
  case mbMessage of
    Just _ -> pure () -- desired result: (Just msg, _)
    Nothing -> do
      Handlers.Logger.logMessage
        logHandle
        Debug
        "Dispatcher. No new unanswered messages from the client\n"
      loop -- desired result: (Just msg, _)
      -- Ask for new message from the client
      where
        loop = do
          fetchedMessage <- Handlers.Client.fetch clientHandle lastMsg
          case fetchedMessage of
            Nothing -> loop
            Just msg -> Handlers.Base.saveMessage baseHandle msg
