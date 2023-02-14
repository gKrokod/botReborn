module Base (newBaseMessage, updateUser, findUser, saveMessage, newBaseUser, readStackMessage, eraseMessage) where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar, threadDelay)
import qualified Data.Map.Strict as Map
import Types (LastMessage, Message, RepeatCount, User)

-- import qualified Data.ByteString.Lazy.Char8 as LC
-- import qualified Data.ByteString.Lazy as L
-- import Data.Map.Internal.Debug (showTree)

mks :: Int
mks = 10 -- for function readStackMessage

type UserDB = Map.Map User RepeatCount

newtype UserDataBase = UserDataBase (MVar UserDB)

type MessageDB = (Maybe Message, Maybe LastMessage)

newtype StackMessage = StackMessage (MVar MessageDB)

newBaseMessage :: IO StackMessage
newBaseMessage = do
  m <- newMVar (Nothing, Nothing)
  return $ StackMessage m

newBaseUser :: IO UserDataBase
newBaseUser = do
  m <- newMVar Map.empty
  return $ UserDataBase m

updateUser :: UserDataBase -> User -> RepeatCount -> IO ()
updateUser (UserDataBase m) user count = do
  base <- takeMVar m
  let base' = Map.insert user count base
  putMVar m base'
  -- for test L.writeFile "config/base.db" (LC.pack $ showTree base')
  seq base' (return ())

findUser :: UserDataBase -> User -> IO (Maybe RepeatCount)
findUser (UserDataBase m) user = do
  base <- takeMVar m
  putMVar m base
  return $ Map.lookup user base

readStackMessage :: StackMessage -> IO MessageDB
readStackMessage (StackMessage m) = do
  threadDelay (mks) -- prevention
  a <- takeMVar m
  putMVar m a
  return a

saveMessage :: StackMessage -> Message -> IO ()
saveMessage (StackMessage m) msg = do
  (mbMessage, lastMessage) <- takeMVar m
  case mbMessage of
    Nothing -> putMVar m (Just msg, lastMessage)
    Just _ -> pure () -- for test (do putMVar m (mbMessage, lastMessage); error "can't save Message")

eraseMessage :: StackMessage -> Message -> IO ()
eraseMessage (StackMessage m) msg = do
  (mbMessage, _) <- takeMVar m
  case mbMessage of
    Nothing -> pure () -- for test (do putMVar m (mbMessage, lastMessage); error "nothing erase ")
    Just msg' ->
      if msg' == msg
        then putMVar m (Nothing, Just msg)
        else pure () -- for test (do putMVar m (mbMessage, lastMessage); error "can't error . Don't mine")
