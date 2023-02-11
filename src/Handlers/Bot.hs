module Handlers.Bot where
import Types
-- import Data.Function ((&))
import qualified Handlers.Base
import Data.Char (isDigit)
import qualified Data.Text as T (Text, pack, unpack, null, all)
import qualified Handlers.Logger

data Handle m = Handle
  {  getMessage :: m (Message)
  ,  sendMessage :: Message -> m ()
  ,  base :: Handlers.Base.Handle m
  ,  helpMessage :: T.Text
  ,  repeatMessage :: T.Text
  ,  logger :: Handlers.Logger.Handle m  
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
  Handlers.Logger.logMessage logHandle 
    Debug "Bot. The bot analyzes the received message"
  case dataMsg of
    Msg _ -> do
      Handlers.Logger.logMessage logHandle 
        Debug "Bot. The received message is text message"
      count <- Handlers.Base.giveRepeatCountFromBase (base h) user 
      mapM_ (sendMessage h) (replicate count msg)
    Gif _ -> do
      Handlers.Logger.logMessage logHandle 
        Debug "Bot. The received message is gif message"
      count <- Handlers.Base.giveRepeatCountFromBase (base h) user 
      mapM_ (sendMessage h) (replicate count msg)
    Command t -> do
      Handlers.Logger.logMessage logHandle
        Debug "Bot. The received message is command message"
      case t of
        "/help" -> sendMessage h (msg {mData = Msg $ helpMessage h})
        "/repeat" -> changeRepeatCountForUser h user
        -- _ -> error "unknow command"
    Query i -> do 
      Handlers.Logger.logMessage logHandle
        Debug "Bot. The received message is query message for change number of repeats for user"
      Handlers.Base.updateUser (base h) user i
    KeyboardMenu -> pure ()
    -- _ -> do
    --   Handlers.Logger.logMessage logHandle Error "The received message is unknwon message"
    --   error "unknow mData Message"
    where dataMsg = mData msg
          id = mID msg
          user = mUser msg

changeRepeatCountForUser :: (Monad m) => Handle m -> User -> m ()
changeRepeatCountForUser h user = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Debug "Bot. Get number of repeats for user from the database"
  count <- Handlers.Base.giveRepeatCountFromBase (base h) user
  let msg = Message {mUser = user}
  sendMessage h (msg {mData = Msg $ (repeatMessage h) <> T.pack (show count) }) 
  sendMessage h (msg {mData = KeyboardMenu}) 
  answer <- getMessage h
  if not (isCorrectRepeatCount answer)
  then do
    Handlers.Logger.logMessage logHandle
      Warning "Bot. The user entered the wrong number of repeats"
    changeRepeatCountForUser h user
  else do
    case mData answer of
      Msg t -> do
        let query' = read $ T.unpack t :: DataFromButton
        makeReaction h (msg {mData = Query query', mUser = mUser answer})
      Query i -> makeReaction h (msg {mData = Query i, mUser = mUser answer})
      -- _ -> do
      --   Handlers.Logger.logMessage logHandle 
      --     Error "Bot. The received message is unknwon message"
      --   error "answer uncorrect"

isCorrectRepeatCount :: Message -> Bool
isCorrectRepeatCount m = case mData m of
                           Msg t -> T.all (isDigit) t && not (T.null t) && helper (read $ T.unpack t)
                           Query i -> helper i
                           otherwise -> False
                         where helper :: DataFromButton -> Bool
                               -- helper = (&&) <$> (> 0) <*> (< 6)
			       helper = \d -> d `elem` [1..5] 
