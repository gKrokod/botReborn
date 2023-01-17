module Main (main) where

import Types
import qualified ClientConsole
import qualified Handlers.Bot
import qualified Base
import qualified Handlers.Base
import qualified Config (loadConfig)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  -- load config and make handles
  base <- Base.newBase
  cfg <- Config.loadConfig
  let baseHandle = Handlers.Base.BaseHandle
                   {  Handlers.Base.defaultRepeatCount = cRepeatCount cfg
		   ,  Handlers.Base.findUser = Base.findUser base
		   ,  Handlers.Base.updateUser = Base.updateUser base
		   }
  
  let handle = Handlers.Bot.Handle 
               { Handlers.Bot.getMessage = ClientConsole.fetch
	       , Handlers.Bot.sendMessage = ClientConsole.carryAway
	       , Handlers.Bot.base = baseHandle
	       , Handlers.Bot.helpMessage = cTextMenuHelp cfg
	       , Handlers.Bot.repeatMessage = cTextMenuRepeat cfg}
  -- do logic
  loop handle

loop :: Handlers.Bot.Handle IO -> IO ()
loop h = do
  msg <- Handlers.Bot.getMessage h
  print $ mID msg
  Handlers.Bot.makeReaction h msg
  loop h
