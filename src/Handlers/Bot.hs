module Handlers.Bot where
import Types
import Data.Function ((&))

data Handle m = Handle
  {  getMessage :: m (Message)
  ,  sendMessage :: Message -> m ()
  ,  defaultRepeatCount :: RepeatCount
  ,  findUser :: User -> m (Maybe RepeatCount)
  ,  updateUser :: User -> RepeatCount -> m ()
  }
 
makeReaction :: (Monad m) => Handle m -> Message -> m ()
makeReaction h msg = case dataMsg of
		      Msg _ -> do
			count <- giveRepeatCountFromBase h user 
			mapM_ (sendMessage h) (replicate count msg)
		      Gif _ -> do
			count <- giveRepeatCountFromBase h user 
			mapM_ (sendMessage h) (replicate count msg)
		      Command t -> case t of
		        commandHelp -> sendMessage h msg
			commandRepeat -> do
			   sendMessage h msg
			   sendMessage h (msg {mData = Keyboard keyboardMenu})
			_ -> error "unknow command"
		      Query i -> updateUser h user i
		      Keyboard _ -> pure ()
		      otherwise -> error "unknow mData Message"
  where dataMsg = mData msg
        id = mID msg
	user = mUser msg

giveRepeatCountFromBase :: (Monad m) => Handle m -> User -> m (RepeatCount)
giveRepeatCountFromBase h user = do
  existUser <- findUser h user
  case existUser of
    Nothing -> do
      updateUser h user (h & defaultRepeatCount)
      giveRepeatCountFromBase h user
    Just repeatCount -> pure repeatCount
