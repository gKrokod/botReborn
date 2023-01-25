module Main (main) where
import Control.Monad
import Control.Concurrent
import Types
import qualified Handlers.Bot
-- import qualified ConsoleBot
import qualified Handlers.Base
import qualified Base
import qualified Handlers.Client
import qualified ClientConsole
import qualified Handlers.Dispatcher
import qualified Dispatcher
import qualified Handlers.Logger
import qualified Logger
import qualified Config (loadConfig)
import System.IO
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO


main :: IO ()
main = do
--bracn good 
  hGetBuffering stdin >>= print 
  hGetBuffering stdout >>= print
  hSetBuffering stdin LineBuffering -- чтобы логи нормально выводились с потоками.
  hSetBuffering stdout LineBuffering
  -- hGetBuffering stdin >>= print 
  -- hGetBuffering stdout >>= print
  -- load config and make handles
-------------------------------------------------------------------------------------------------
  stackMessage <- Base.newBaseMessage
  base <- Base.newBaseUser
  cfg <- Config.loadConfig

  let logHandle = Handlers.Logger.Handle
                  { Handlers.Logger.levelLogger = cLvlLog cfg -- Fatal
		  , Handlers.Logger.writeLog = Logger.writeLog
		  }

  let baseHandle = Handlers.Base.Handle
                   {  Handlers.Base.defaultRepeatCount = cRepeatCount cfg
		   ,  Handlers.Base.readStackMessage = Base.readStackMessage stackMessage
		   ,  Handlers.Base.saveMessage = Base.saveMessage stackMessage
		   ,  Handlers.Base.eraseMessage = Base.eraseMessage stackMessage
		   ,  Handlers.Base.findUser = Base.findUser base
		   ,  Handlers.Base.updateUser = Base.updateUser base
		   ,  Handlers.Base.logger = logHandle
		   }
  
  let clientHandle = Handlers.Client.Handle
                     { Handlers.Client.fetch = ClientConsole.fetch
		     , Handlers.Client.carryAway = ClientConsole.carryAway
		     , Handlers.Client.logger = logHandle
	             }

  let botHandle = Handlers.Bot.Handle 
		  { Handlers.Bot.getMessage = undefined --Handlers.Dispatcher.getMessage dispatcherHandle 1
		  , Handlers.Bot.sendMessage = ClientConsole.carryAway
		  , Handlers.Bot.base = baseHandle
		  , Handlers.Bot.helpMessage = cTextMenuHelp cfg
		  , Handlers.Bot.repeatMessage = cTextMenuRepeat cfg
		  , Handlers.Bot.logger = logHandle
		  }

  let handle = Handlers.Dispatcher.Handle
	       { Handlers.Dispatcher.client = clientHandle
	       , Handlers.Dispatcher.bot = botHandle
	       , Handlers.Dispatcher.forkForUser = Dispatcher.forkForUser
	       , Handlers.Dispatcher.logger = logHandle
	       }

  forkIO $ forever (do Handlers.Dispatcher.watcherForNewMessage handle; threadDelay (100))
  print "Watcher is running"
  print "Dispatcher is running"
  forever 
    $ Handlers.Dispatcher.dispatcher handle 

