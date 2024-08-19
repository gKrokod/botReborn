module Handlers.Bot (Handle (..), doWork, changeRepeatCountForUser, isCorrectRepeatCount, makeReaction) where

import Control.Monad (replicateM_, void)
import Data.Char (isDigit)
import qualified Data.Text as T (Text, all, null, pack, unpack)
import qualified Handlers.Base
import qualified Handlers.Logger
import Types (Data (..), DataFromButton, Log (..), Message (..), User, defaultMessage)

data Handle m = Handle
  { getMessage :: m Message,
    sendMessage :: Message -> m (),
    base :: Handlers.Base.Handle m,
    helpMessage :: T.Text,
    repeatMessage :: T.Text,
    logger :: Handlers.Logger.Handle m
  }

doWork :: (Monad m) => Handle m -> m ()
doWork h = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Debug "Bot. Run the bot logic: get message -> make reaction"
  message <- getMessage h
  makeReaction h message

---------------------------------------------------------------------------------------------------------
makeReaction :: (Monad m) => Handle m -> Message -> m ()
makeReaction h msg = do
  let logHandle = logger h
  Handlers.Logger.logMessage
    logHandle
    Debug
    "Bot. The bot analyzes the received message"
  case dataMsg of
    Msg _ -> do
      Handlers.Logger.logMessage
        logHandle
        Debug
        "Bot. The received message is text message"
      count <- Handlers.Base.giveRepeatCountFromBase (base h) user
      replicateM_ count (sendMessage h msg)
    Gif _ -> do
      Handlers.Logger.logMessage
        logHandle
        Debug
        "Bot. The received message is gif message"
      count <- Handlers.Base.giveRepeatCountFromBase (base h) user
      replicateM_ count (sendMessage h msg)
    Command t -> do
      Handlers.Logger.logMessage
        logHandle
        Debug
        "Bot. The received message is command message"
      case t of
        "/help" -> sendMessage h (msg {mData = Msg $ helpMessage h})
        "/repeat" -> changeRepeatCountForUser h user
        _ -> void $ Handlers.Logger.logMessage logHandle Error "Bot. The received command isn't command message"
    Query i -> do
      Handlers.Logger.logMessage
        logHandle
        Debug
        "Bot. The received message is query message for change number of repeats for user"
      Handlers.Base.updateUser (base h) user i
    KeyboardMenu -> pure ()
  where
    dataMsg = mData msg
    user = mUser msg

changeRepeatCountForUser :: (Monad m) => Handle m -> User -> m ()
changeRepeatCountForUser h user = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Debug "Bot. Get number of repeats for user from the database"
  count <- Handlers.Base.giveRepeatCountFromBase (base h) user
  let msg = defaultMessage {mUser = user}
  sendMessage h (msg {mData = Msg $ repeatMessage h <> T.pack (show count)})
  sendMessage h (msg {mData = KeyboardMenu})
  answer <- getMessage h
  if not (isCorrectRepeatCount answer)
    then do
      Handlers.Logger.logMessage
        logHandle
        Warning
        "Bot. The user entered the wrong number of repeats"
      changeRepeatCountForUser h user
    else do
      case mData answer of
        Msg t -> do
          let query' = read $ T.unpack t :: DataFromButton
          makeReaction h (msg {mData = Query query', mUser = mUser answer})
        Query i -> makeReaction h (msg {mData = Query i, mUser = mUser answer})
        _ -> do
          Handlers.Logger.logMessage logHandle Error "Bot. The received message is unknown message"
          pure ()

isCorrectRepeatCount :: Message -> Bool
isCorrectRepeatCount m = case mData m of
  Msg t -> T.all isDigit t && not (T.null t) && helper (read $ T.unpack t)
  Query i -> helper i
  _ -> False
  where
    helper :: DataFromButton -> Bool
    helper d = d `elem` [1 .. 5]
