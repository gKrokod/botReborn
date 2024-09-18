module Initialization (initialization) where

import qualified Base
import qualified Clients.Console
import qualified Clients.Telegram
import Config (Config (..), Mode (..), loadConfig)
import Data.Bool (bool)
import qualified Dispatcher
import qualified Handlers.Base
import qualified Handlers.Bot
import qualified Handlers.Client
import qualified Handlers.Dispatcher
import qualified Handlers.Logger
import qualified Logger
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)
import Types (RepeatCount (..))

initialization :: IO (Handlers.Dispatcher.Handle IO)
initialization = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  stackMessage <- Base.newBaseMessage
  base <- Base.newBaseUser
  cfg <- Config.loadConfig
  print cfg
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = cLvlLog cfg,
            Handlers.Logger.writeLog = Logger.writeLog
          }

  let baseHandle =
        Handlers.Base.Handle
          { Handlers.Base.defaultRepeatCount = RepeatCount $ cRepeatCount cfg,
            Handlers.Base.readStackMessage = Base.readStackMessage stackMessage,
            Handlers.Base.saveMessage = Base.saveMessage stackMessage,
            Handlers.Base.eraseMessage = Base.eraseMessage stackMessage,
            Handlers.Base.findUser = Base.findUser base,
            Handlers.Base.updateUser = Base.updateUser base,
            Handlers.Base.logger = logHandle
          }

  let clientHandle =
        Handlers.Client.Handle
          { Handlers.Client.fetch =
              bool
                (Clients.Telegram.fetch cfg)
                Clients.Console.fetch
                (ConsoleBot == cMode cfg),
            Handlers.Client.carryAway =
              bool
                (Clients.Telegram.carryAway cfg)
                Clients.Console.carryAway
                (ConsoleBot == cMode cfg),
            Handlers.Client.logger = logHandle
          }

  let botHandle =
        Handlers.Bot.Handle
          { Handlers.Bot.base = baseHandle,
            Handlers.Bot.helpMessage = cTextMenuHelp cfg,
            Handlers.Bot.repeatMessage = cTextMenuRepeat cfg,
            Handlers.Bot.client = clientHandle,
            Handlers.Bot.logger = logHandle
          }

  pure $
    Handlers.Dispatcher.Handle
      { Handlers.Dispatcher.forkForUser = Dispatcher.forkForUser,
        Handlers.Dispatcher.bot = botHandle,
        Handlers.Dispatcher.logger = logHandle
      }
