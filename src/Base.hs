module Base where
-- здесь реализация базы данных пользователей на потоках в мапке
-- import qualified Handlers.Base as Handler
import Types (User, RepeatCount)
import Control.Concurrent
import qualified Data.Map.Strict as Map

type UserDB = Map.Map User RepeatCount
newtype UserDataBase = UserDataBase (MVar UserDB)
  
newBase :: IO UserDataBase
newBase = do
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


-- close :: Handler.Handle IO -> ()
close = undefined

