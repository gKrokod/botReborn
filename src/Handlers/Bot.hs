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
  Handlers.Logger.logMessage logHandle Debug "Запускаем логику бота: запрос сообщений - ответ"
  message <- getMessage h
  makeReaction h message

---------------------------------------------------------------------------------------------------------
makeReaction :: (Monad m) => Handle m -> Message -> m ()
makeReaction h msg = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Debug "бот рассматривает поступивщее сообщение"
  case dataMsg of
    Msg _ -> do
      Handlers.Logger.logMessage logHandle Debug "Боту было передано текстовое сообщение"
      count <- Handlers.Base.giveRepeatCountFromBase (base h) user 
      mapM_ (sendMessage h) (replicate count msg)
    Gif _ -> do
      Handlers.Logger.logMessage logHandle Debug "Боту было передано gif сообщение"
      count <- Handlers.Base.giveRepeatCountFromBase (base h) user 
      mapM_ (sendMessage h) (replicate count msg)
    Command t -> do
      Handlers.Logger.logMessage logHandle Debug "Боту было передана команда"
      case t of
        "/help" -> sendMessage h (msg {mData = Msg $ helpMessage h})
        "/repeat" -> changeRepeatCountForUser h user
        _ -> error "unknow command"
    Query i -> do 
      Handlers.Logger.logMessage logHandle Debug "Боту было передан ответ на запрос о количестве повторений"
      Handlers.Base.updateUser (base h) user i
    KeyboardMenu -> pure ()
    otherwise -> do
      Handlers.Logger.logMessage logHandle Error "Пришло неизвестное сообщение"
      error "unknow mData Message"
    where dataMsg = mData msg
          id = mID msg
          user = mUser msg

changeRepeatCountForUser :: (Monad m) => Handle m -> User -> m ()
changeRepeatCountForUser h user = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Debug "запрашиваем количество повторений в базе для пользователя"
  count <- Handlers.Base.giveRepeatCountFromBase (base h) user
  let msg = Message {mUser = user} --когда команда /repeat, почему-то стало вылетать здесь
  sendMessage h (msg {mData = Msg $ (repeatMessage h) <> T.pack (show count) }) 
  sendMessage h (msg {mData = KeyboardMenu}) 
  answer <- getMessage h
  if not (isCorrectRepeatCount answer)
  then do
    Handlers.Logger.logMessage logHandle Warning "Пользователь вводит некорректное значение количества повторов"
    changeRepeatCountForUser h user
  else do
    case mData answer of
      Msg t -> do
        let query' = read $ T.unpack t :: DataFromButton
	makeReaction h (msg {mData = Query query', mUser = mUser answer})
      Query i -> makeReaction h (msg {mData = Query i, mUser = mUser answer})
      otherwise -> do
        Handlers.Logger.logMessage logHandle Error "Пришло неизвестное сообщение"
        error "answer uncorrect"

isCorrectRepeatCount :: Message -> Bool
isCorrectRepeatCount m = case mData m of
                           Msg t -> T.all (isDigit) t && not (T.null t) && helper (read $ T.unpack t)
			   Query i -> helper i
			   otherwise -> False
			 where helper :: DataFromButton -> Bool
			       helper = (&&) <$> (> 0) <*> (< 6)
