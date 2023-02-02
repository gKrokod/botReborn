module Main (main) where
import Control.Monad
import Data.Aeson 
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
import Data.Bool (bool)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import ClientVK.HttpMessage
import Network.HTTP.Simple --(parseRequest, Request, httpLBS, getResponseBody, getResponseStatusCode, getResponseHeader)


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
  print "Start Programm"

  let logHandle = Handlers.Logger.Handle
                  { Handlers.Logger.levelLogger = cLvlLog cfg -- Fatal
                  , Handlers.Logger.writeLog = Logger.writeLog
                  }

  print $ buildGetRequest (cfg)
  response <- httpLBS $ buildGetRequest (cfg)
  let status = getResponseStatusCode response
  case status of
    404 -> Handlers.Logger.logMessage logHandle Fatal "Bot Server not found"
    301 -> Handlers.Logger.logMessage logHandle Fatal "Bot Server Moved Permanently"
    200 -> do  
      Handlers.Logger.logMessage logHandle Debug "Bot Server give us response"
      print $ getResponseHeader "Content-Type" response
      print $ getResponseBody response
      let jsonBody = getResponseBody (response)
      let mbMessage = decode jsonBody :: Maybe Message
      case mbMessage of
        Just m -> do
	  Handlers.Logger.logMessage logHandle Debug "Get message"
	  print m
	Nothing -> do 
	  print "nothing"
          let mbMessage = eitherDecode jsonBody :: Either String Message
	  print $ mbMessage

--   
--   let logHandle = Handlers.Logger.Handle
--                   { Handlers.Logger.levelLogger = cLvlLog cfg -- Fatal
--                   , Handlers.Logger.writeLog = Logger.writeLog
--                   }
--
--   let baseHandle = Handlers.Base.Handle
--                    {  Handlers.Base.defaultRepeatCount = cRepeatCount cfg
--                    ,  Handlers.Base.readStackMessage = Base.readStackMessage stackMessage
--                    ,  Handlers.Base.saveMessage = Base.saveMessage stackMessage
--                    ,  Handlers.Base.eraseMessage = Base.eraseMessage stackMessage
--                    ,  Handlers.Base.findUser = Base.findUser base
--                    ,  Handlers.Base.updateUser = Base.updateUser base
--                    ,  Handlers.Base.logger = logHandle
--                    }
--    
--   let clientHandle = Handlers.Client.Handle
--                      { Handlers.Client.fetch = bool ClientConsole.fetch
--                                                     ClientConsole.fetch 
--                                                     (ConsoleBot == cMode cfg)
--                      , Handlers.Client.carryAway = bool ClientConsole.carryAway
--                                                         ClientConsole.carryAway 
--                                                         (ConsoleBot == cMode cfg) 
--                      , Handlers.Client.logger = logHandle
--                      }
--
--   let botHandle = Handlers.Bot.Handle 
--                   { Handlers.Bot.getMessage = undefined --Handlers.Dispatcher.getMessage dispatcherHandle 1
--                   , Handlers.Bot.sendMessage = ClientConsole.carryAway
--                   , Handlers.Bot.base = baseHandle
--                   , Handlers.Bot.helpMessage = cTextMenuHelp cfg
--                   , Handlers.Bot.repeatMessage = cTextMenuRepeat cfg
--                   , Handlers.Bot.logger = logHandle
--                   }
--
--   let handle = Handlers.Dispatcher.Handle
--                { Handlers.Dispatcher.client = clientHandle
--                , Handlers.Dispatcher.bot = botHandle
--                , Handlers.Dispatcher.forkForUser = Dispatcher.forkForUser
--                , Handlers.Dispatcher.logger = logHandle
--                }
-- -- start watcher for new message
--   _ <- forkIO 
--     $ forever $ Handlers.Dispatcher.watcherForNewMessage handle
-- -- start main logic
--   forever 
--     $ Handlers.Dispatcher.dispatcher handle 
--
