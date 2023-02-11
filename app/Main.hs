module Main (main) where
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import System.IO (hSetBuffering, stdout, stdin, BufferMode(..))
import Types (Config(..), Mode(..))
import qualified Handlers.Bot
import qualified Handlers.Base
import qualified Base
import qualified Handlers.Client
import qualified ClientConsole
import qualified ClientTM
import qualified Handlers.Dispatcher
import qualified Dispatcher
import qualified Handlers.Logger
import qualified Logger
import qualified Config (loadConfig)
import Data.Bool (bool)


main :: IO ()
main = do
  -- set buffering 
  hSetBuffering stdin LineBuffering 
  hSetBuffering stdout LineBuffering
  -- load config and make handles
-------------------------------------------------------------------------------------------------
  stackMessage <- Base.newBaseMessage
  base <- Base.newBaseUser
  cfg <- Config.loadConfig

  let logHandle = Handlers.Logger.Handle
                  { Handlers.Logger.levelLogger = cLvlLog cfg --Debug
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
  
  let botHandle = Handlers.Bot.Handle 
                  { Handlers.Bot.base = baseHandle
                  , Handlers.Bot.helpMessage = cTextMenuHelp cfg
                  , Handlers.Bot.repeatMessage = cTextMenuRepeat cfg
                  , Handlers.Bot.logger = logHandle
                  }

  let clientHandle = Handlers.Client.Handle
                     { Handlers.Client.fetch = bool (ClientTM.fetch cfg)
                                                    ClientConsole.fetch
                                                    (ConsoleBot == cMode cfg)
                     , Handlers.Client.carryAway = bool (ClientTM.carryAway cfg)
                                                        ClientConsole.carryAway 
                                                        (ConsoleBot == cMode cfg) 
                     , Handlers.Client.logger = logHandle
                     }

  let handle = Handlers.Dispatcher.Handle
               { Handlers.Dispatcher.forkForUser = Dispatcher.forkForUser
               , Handlers.Dispatcher.client = clientHandle
               , Handlers.Dispatcher.bot = botHandle
               , Handlers.Dispatcher.logger = logHandle
               }
-------------------------------------------------------------------------------------------------
-- start watcher for new messages
  _ <- forkIO 
    $ forever $ Handlers.Dispatcher.watcherForNewMessage handle
-- start main logic
  forever 
    $ Handlers.Dispatcher.dispatcher handle 
