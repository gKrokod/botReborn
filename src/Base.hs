module Base where
-- здесь реализация базы данных пользователей на потоках в мапке
-- import qualified Handlers.Base as Handler
import Types (User, RepeatCount, Message, LastMessage)
import Control.Concurrent
import qualified Data.Map.Strict as Map

mks :: Int
mks = 1 -- for function readStackMessage

type UserDB = Map.Map User RepeatCount
newtype UserDataBase = UserDataBase (MVar UserDB)


type MessageDB = (Maybe Message, Maybe LastMessage)
newtype MessageDataBase = MessageDataBase (MVar MessageDB)

newBaseMessage :: IO MessageDataBase
newBaseMessage = do
  m <- newMVar (Nothing, Nothing)
  return $ MessageDataBase m

newBaseUser :: IO UserDataBase
newBaseUser = do
  m <- newMVar Map.empty
  return $ UserDataBase m
      
updateUser :: UserDataBase -> User -> RepeatCount -> IO ()
updateUser (UserDataBase m) user count = do
  base <- takeMVar m
  let base' = Map.insert user count base
  putMVar m base'
  seq base' (return ())
    
findUser :: UserDataBase -> User -> IO (Maybe RepeatCount)
findUser (UserDataBase m) user = do
  base <- takeMVar m
  putMVar m base
  return $ Map.lookup user base
-- просто считываем первое - пока не взятое в обработку сообщение и второе, сообщение на которое мы уже запустили реакцию.
readStackMessage :: MessageDataBase -> IO MessageDB
readStackMessage (MessageDataBase m) = do
  threadDelay (mks) -- for Wathcer in Dispatcher and another loop
  a <- takeMVar m
  putMVar m a
  return a
-- если в стеке ничего не хранится, то сохраняем в него, есле же там лежит сообщение, то пишем ошибку, либо перезапускаем сохранение, ждя когда там станет nothing
saveMessage :: MessageDataBase -> Message -> IO ()
saveMessage (MessageDataBase m) msg = do
  (stack, lastMessage) <- takeMVar m
  case stack of
    Nothing -> putMVar m (Just msg, lastMessage)
    Just _ -> (do putMVar m (stack, lastMessage); error "can't save message")
-- получаем сообщение, смотрим, что хранится в стеке. Если сообщение наше, то убираем его, чтобы обработать
-- и записываем в стек, что оно пока последнее, которое мы стали обрабатывать.лоЖц
--
eraseMessage :: MessageDataBase -> Message -> IO ()
eraseMessage (MessageDataBase m) msg = do
  (stack, lastMessage) <- takeMVar m
  case stack of
    Nothing -> (do putMVar m (stack, lastMessage); error "nothing erase message")
    Just msg' -> if msg' == msg
                 then putMVar m (Nothing, Just msg)
                 else (do putMVar m (stack, lastMessage); error "can't error message. Don't mine")


-- close :: Handler.Handle IO -> ()
close = undefined

