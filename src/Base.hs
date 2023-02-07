module Base where
-- здесь реализация базы данных пользователей на потоках в мапке
import Types (User, RepeatCount, Message, LastMessage)
import Control.Concurrent (takeMVar, MVar, putMVar,threadDelay, newMVar)
import qualified Data.Map.Strict as Map
-- import qualified Data.ByteString.Lazy.Char8 as LC
-- import qualified Data.ByteString.Lazy as L
-- import Data.Map.Internal.Debug (showTree)

mks :: Int
mks = 10 -- for function readStackMessage

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
  -- L.writeFile "config/base.db" (LC.pack $ showTree base')
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
  (mbMessage, lastMessage) <- takeMVar m
  case mbMessage of
    Nothing -> putMVar m (Just msg, lastMessage)
    Just _ -> (do putMVar m (mbMessage, lastMessage); error "can't save Message")
-- получаем сообщение, смотрим, что хранится в стеке. Если сообщение наше, то убираем его, чтобы обработать
-- и записываем в стек, что оно пока последнее, которое мы стали обрабатывать.лоЖц
--
eraseMessage :: MessageDataBase -> Message -> IO ()
eraseMessage (MessageDataBase m) msg = do
  (mbMessage, lastMessage) <- takeMVar m
  case mbMessage of
    Nothing -> (do putMVar m (mbMessage, lastMessage); error "nothing erase Message")
    Just msg' -> if msg' == msg
                 then putMVar m (Nothing, Just msg)
                 else (do putMVar m (mbMessage, lastMessage); error "can't error Message. Don't mine")
