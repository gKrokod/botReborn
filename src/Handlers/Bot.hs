module Handlers.Bot where
import Types
-- import Data.Function ((&))
import qualified Handlers.Base
import Data.Char (isDigit)
import qualified Data.Text as T (Text, pack, unpack, null, all)

data Handle m = Handle
  {  getMessage :: m (Message)
  ,  sendMessage :: Message -> m ()
  ,  base :: Handlers.Base.Handle m
  ,  helpMessage :: T.Text
  ,  repeatMessage :: T.Text
  }

doWork :: (Monad m) => Handle m -> m ()
doWork h = do
  message <- getMessage h
  makeReaction h message

---------------------------------------------------------------------------------------------------------
makeReaction :: (Monad m) => Handle m -> Message -> m ()
makeReaction h msg = case dataMsg of
		      Msg _ -> do
			count <- Handlers.Base.giveRepeatCountFromBase (base h) user 
			mapM_ (sendMessage h) (replicate count msg)
		      Gif _ -> do
			count <- Handlers.Base.giveRepeatCountFromBase (base h) user 
			mapM_ (sendMessage h) (replicate count msg)
		      Command t -> case t of
		        "/help" -> sendMessage h (msg {mData = Msg $ helpMessage h})
			"/repeat" -> changeRepeatCountForUser h user
			_ -> error "unknow command"
		      Query i -> Handlers.Base.updateUser (base h) user i
		      KeyboardMenu -> pure ()
		      otherwise -> error "unknow mData Message"
  where dataMsg = mData msg
        id = mID msg
	user = mUser msg

changeRepeatCountForUser :: (Monad m) => Handle m -> User -> m ()
changeRepeatCountForUser h user = do
  count <- Handlers.Base.giveRepeatCountFromBase (base h) user
  -- let msg = Message {mUser = user, mData = KeyboardMenu, mID = 0}
  let msg = Message {mUser = user} --когда команда /repeat, почему-то стало вылетать здесь
  sendMessage h (msg {mData = Msg $ (repeatMessage h) <> T.pack (show count) }) 
  sendMessage h (msg {mData = KeyboardMenu}) 
  answer <- getMessage h
  if not (isCorrectRepeatCount answer)
  then changeRepeatCountForUser h user
  else do
    case mData answer of
      Msg t -> do
        let query' = read $ T.unpack t :: DataFromButton
	makeReaction h (msg {mData = Query query', mUser = mUser answer})
      Query i -> makeReaction h (msg {mData = Query i, mUser = mUser answer})
      otherwise -> error "answer uncorrect"

isCorrectRepeatCount :: Message -> Bool
isCorrectRepeatCount m = case mData m of
                           Msg t -> T.all (isDigit) t && not (T.null t) && helper (read $ T.unpack t)
			   Query i -> helper i
			   otherwise -> False
			 where helper :: DataFromButton -> Bool
			       helper = (&&) <$> (> 0) <*> (< 6)
