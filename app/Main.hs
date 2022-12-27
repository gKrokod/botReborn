module Main (main) where

import Lib
import Types
import qualified Bot
import Handlers.Bot
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  let m = Message { mData = Query 333332, mID = 1, mUser = 12345 }
  case mData m of
     Msg a -> TIO.putStrLn (a)
     Query a -> print a 
  let handle = Handlers.Bot.Handle 
               { getMessage = Bot.getMessage
	       , sendMessage = Bot.sendMessage
	       , defaultRepeatCount = Bot.defaultRepeatCount
	       , findUser = Bot.findUser
	       , updateUser = Bot.updateUser}
  msg <- getMessage handle
  makeReaction handle msg
  return ()
