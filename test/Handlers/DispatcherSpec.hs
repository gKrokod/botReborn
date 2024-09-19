module Handlers.DispatcherSpec (spec) where

import Config (Config (..))
import Control.Monad (void)
import Control.Monad.State (State, evalState, get, modify, put)
import qualified Data.Map.Strict as Map (Map, fromList, insert, lookup)
import qualified Data.Text as T
import qualified Handlers.Base (Handle (..))
import qualified Handlers.Bot (Handle (..),  getMessage )
import qualified Handlers.Client (Handle (..))
import qualified Handlers.Logger (Handle (..))
import Test.Hspec (Spec, it, shouldBe)
import Types (Data (..), ID (..), LastMessage, Log (..), Message (..), RepeatCount (..), User (..))
import Handlers.Dispatcher

spec :: Spec
spec = do
    it "Take message from client if stackMessages haven't a new message" $ do
      let testUser = User 341 :: User
      let testText = "some message"
      let testMessage = Message {mData = Msg testText, mID = ID 1, mUser = testUser}
      let testStack = (Nothing, Nothing) :: (Maybe Message, Maybe LastMessage)

      let logerHandle =
            Handlers.Logger.Handle
              { Handlers.Logger.levelLogger = Debug,
                Handlers.Logger.writeLog = \_ -> pure ()
              } ::
              Handlers.Logger.Handle (State (Maybe Message, Maybe LastMessage))
      let baseHandle =
            Handlers.Base.Handle
              { Handlers.Base.readStackMessage = get,
                Handlers.Base.saveMessage = \message -> void $ put (Just message, Nothing),
                Handlers.Base.logger = logerHandle
              }
      let clientHandle =
            Handlers.Client.Handle
              { Handlers.Client.fetch = \_ -> pure $ Just testMessage,
                Handlers.Client.carryAway = \_ -> pure (),
                Handlers.Client.logger = logerHandle
              }
      let botHandle =
            Handlers.Bot.Handle
              { Handlers.Bot.base = baseHandle,
                Handlers.Bot.logger = logerHandle,
                Handlers.Bot.client = clientHandle
              }
      let dispatcherHandle =
            Handlers.Dispatcher.Handle
              { Handlers.Dispatcher.bot = botHandle,
                Handlers.Dispatcher.logger = logerHandle
              }
      evalState
        ( Handlers.Dispatcher.watcherForNewMessage dispatcherHandle
            >> Handlers.Bot.getMessage botHandle testUser
        )
        testStack
        `shouldBe` testMessage

    it "Save a new user in Base and start fork for him because of catch his message" $ do
      let testUser = User 341 :: User
      let newUser = User 444 :: User
      let repeatCount = RepeatCount 5 :: RepeatCount
      let forkCount = RepeatCount 111 :: RepeatCount
      let testBase = Map.fromList [(testUser, repeatCount)]
      let testBase2 = Map.fromList [(testUser, repeatCount), (newUser, repeatCount)]
      let testText = "some message"
      let testMessage = Message {mData = Msg testText, mID = ID 1, mUser = newUser}
      let testStack = (Just testMessage, Nothing) :: (Maybe Message, Maybe LastMessage)
      let testForkImitation = User 11111111 :: User
      let logerHandle =
            Handlers.Logger.Handle
              { Handlers.Logger.levelLogger = Debug,
                Handlers.Logger.writeLog = \_ -> pure ()
              } ::
              Handlers.Logger.Handle (State (Map.Map User RepeatCount))
      let baseHandle =
            Handlers.Base.Handle
              { Handlers.Base.readStackMessage = pure testStack,
                Handlers.Base.logger = logerHandle,
                Handlers.Base.defaultRepeatCount = RepeatCount $ cRepeatCount testConfig,
                Handlers.Base.findUser = \user -> get >>= \base -> pure $ Map.lookup user base,
                Handlers.Base.updateUser = \user count -> void $ modify (Map.insert user count)
              }
      let clientHandle =
            Handlers.Client.Handle
              { Handlers.Client.logger = logerHandle
              }
      let botHandle =
            Handlers.Bot.Handle
              { Handlers.Bot.base = baseHandle,
                Handlers.Bot.client = clientHandle,
                Handlers.Bot.logger = logerHandle
              }

      let dispatcherHandle =
            Handlers.Dispatcher.Handle
              { Handlers.Dispatcher.bot = botHandle,
                Handlers.Dispatcher.logger = logerHandle,
                Handlers.Dispatcher.forkForUser = \_ -> void $ modify (Map.insert testForkImitation forkCount)
              }
      -- testBase
      evalState (Handlers.Base.findUser baseHandle newUser) testBase
        `shouldBe` Nothing

      evalState (Handlers.Base.findUser baseHandle testForkImitation) testBase
        `shouldBe` Nothing

      evalState
        ( Handlers.Dispatcher.dispatcher dispatcherHandle
            >> Handlers.Base.findUser baseHandle newUser
        )
        testBase
        `shouldBe` Just (RepeatCount $ cRepeatCount testConfig)

      evalState
        ( Handlers.Dispatcher.dispatcher dispatcherHandle
            >> Handlers.Base.findUser baseHandle testForkImitation
        )
        testBase
        `shouldBe` Just forkCount

      -- testBase2
      evalState (Handlers.Base.findUser baseHandle newUser) testBase2
        `shouldBe` Just repeatCount

      evalState (Handlers.Base.findUser baseHandle testForkImitation) testBase2
        `shouldBe` Nothing

      evalState
        ( Handlers.Dispatcher.dispatcher dispatcherHandle
            >> Handlers.Base.findUser baseHandle newUser
        )
        testBase2
        `shouldBe` Just repeatCount

      evalState
        ( Handlers.Dispatcher.dispatcher dispatcherHandle
            >> Handlers.Base.findUser baseHandle testForkImitation
        )
        testBase2
        `shouldBe` Nothing
testConfig :: Config
testConfig =
  Config
    { cRepeatCount = 3,
      cTextMenuHelp = "Help menu:" :: T.Text,
      cTextMenuRepeat = "Repeat menu:" :: T.Text,
      cLvlLog = Debug :: Log
    }
