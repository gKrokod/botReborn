module Handlers.BotSpec (spec) where

import Config (Config (..))
import Control.Monad (void)
import Control.Monad.State (State, evalState, execState, get, modify, put)
import qualified Data.Map.Strict as Map (Map, fromList, insert, lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Handlers.Base (Handle (..), giveRepeatCountFromBase)
import qualified Handlers.Client (Handle (..))
import qualified Handlers.Logger (Handle (..))
import Test.Hspec (Spec, context, describe,  it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)
import Text.Read (readMaybe)
import Types (Command (..), Data (..), DataFromButton (..), ID (..), LastMessage, Log (..), Message (..), RepeatCount (..), User (..))

import Handlers.Bot

spec :: Spec
spec = do
  describe "Random input:" $ modifyMaxSuccess (const 1000) $ do
    it "User can change his repeat count" $ do
      let oldRepeatCount = RepeatCount 1
      let newRepeatCount = DataFromButton 2
      let testUser = User 111 :: User
      let testBase = Map.fromList [(testUser, oldRepeatCount)]
      let testMessage = Message {mData = Query newRepeatCount, mID = ID 1, mUser = testUser}

      let loggerHandle =
            Handlers.Logger.Handle
              { Handlers.Logger.levelLogger = Debug,
                Handlers.Logger.writeLog = \_ -> pure ()
              } ::
              Handlers.Logger.Handle (State (Map.Map User RepeatCount))
      let baseHandle =
            Handlers.Base.Handle
              { Handlers.Base.findUser = \user -> get >>= \base -> pure $ Map.lookup user base,
                Handlers.Base.updateUser = \user count -> void $ modify (Map.insert user count),
                Handlers.Base.readStackMessage = pure (Just testMessage, Nothing),
                Handlers.Base.eraseMessage = \_ -> pure (),
                Handlers.Base.logger = loggerHandle
              }
      let clientHandle =
            Handlers.Client.Handle
              { Handlers.Client.carryAway = \_ -> pure (),
                Handlers.Client.logger = loggerHandle
              }

      let botHandle =
            Handlers.Bot.Handle
              { Handlers.Bot.repeatMessage = cTextMenuRepeat testConfig,
                Handlers.Bot.helpMessage = cTextMenuHelp testConfig,
                Handlers.Bot.logger = loggerHandle,
                Handlers.Bot.client = clientHandle,
                Handlers.Bot.base = baseHandle
              }

      evalState (Handlers.Base.giveRepeatCountFromBase baseHandle testUser) testBase
        `shouldBe` oldRepeatCount
      evalState
        ( Handlers.Bot.changeRepeatCountForUser botHandle testUser
            >> Handlers.Base.giveRepeatCountFromBase baseHandle testUser
        )
        testBase
        `shouldBe` RepeatCount (dataFromButton newRepeatCount)
    context "User can change repeat count only in range [1..5]" $ do
      it "Input: Int" $ do
        property $ \answer -> do
          let msg = Message {mData = Query (DataFromButton answer), mID = ID 1, mUser = User 2}
          Handlers.Bot.isCorrectRepeatCount msg
            `shouldBe` 1 <= answer && answer <= 5      

      it "Input: Text" $ do
        property $ \answer -> do
          let numberMessage =
                if T.length (T.pack answer) /= 1
                  then 0
                  else fromMaybe 0 (readMaybe answer :: Maybe Int)
          let msg = Message {mData = Msg (T.pack answer), mID = ID 1, mUser = User 2}

          Handlers.Bot.isCorrectRepeatCount msg
            `shouldBe` 1 <= numberMessage && numberMessage <= 5

    context "Correct text in answer on:" $ do
      it "/help" $ do
        property $ \helpMessage -> do
          let testMessage = Message {mData = Service Help, mID = ID 1, mUser = User 2}

          let logHandle =
                Handlers.Logger.Handle
                  { Handlers.Logger.levelLogger = Debug,
                    Handlers.Logger.writeLog = \_ -> pure ()
                  } ::
                  Handlers.Logger.Handle (State T.Text)
          let clientHandle =
                Handlers.Client.Handle
                  { Handlers.Client.carryAway =
                      \(Message {mData = msg}) -> case msg of
                        Msg text -> void $ put text
                        _ -> pure (),
                    Handlers.Client.logger = logHandle
                  }

          let botHandle =
                Handlers.Bot.Handle
                  { Handlers.Bot.client = clientHandle,
                    Handlers.Bot.helpMessage = T.pack helpMessage,
                    Handlers.Bot.logger = logHandle
                  }

          execState (Handlers.Bot.makeReaction botHandle testMessage) "no help menu"
            `shouldBe` T.pack helpMessage

      it "/repeat" $ do
        property $ \repeatMessage -> do
          let testUser = User 2 :: User
          let testMessage = Message {mData = Service Repeat, mID = ID 1, mUser = testUser}
          let repeatCount = RepeatCount 5 :: RepeatCount

          let logHandle =
                Handlers.Logger.Handle
                  { Handlers.Logger.levelLogger = Debug,
                    Handlers.Logger.writeLog = \_ -> pure ()
                  } ::
                  Handlers.Logger.Handle (State T.Text)
          let baseHandle =
                Handlers.Base.Handle
                  { Handlers.Base.findUser = \_ -> pure $ Just repeatCount,
                    Handlers.Base.readStackMessage = pure (Just $ Message {mData = Msg "1", mID = ID 1, mUser = testUser}, Nothing),
                    Handlers.Base.updateUser = \_ _ -> pure (),
                    Handlers.Base.eraseMessage = \_ -> pure ()
                  }
          let clientHandle =
                Handlers.Client.Handle
                  { Handlers.Client.carryAway =
                      \(Message {mData = msg}) -> case msg of
                        Msg text -> void $ put text
                        _ -> pure (),
                    Handlers.Client.logger = logHandle
                  }
          let botHandle =
                Handlers.Bot.Handle
                  { Handlers.Bot.repeatMessage = T.pack repeatMessage,
                    Handlers.Bot.logger = logHandle,
                    Handlers.Bot.client = clientHandle,
                    Handlers.Bot.base = baseHandle
                  }
          execState (Handlers.Bot.makeReaction botHandle testMessage) "no repeat menu"
            `shouldBe` T.pack (repeatMessage <> show repeatCount)

      it "text message and gif message" $ do
        property $ \textMessage -> do
          let testUser = User 2 :: User
          let testMessage = Message {mData = Msg (T.pack textMessage), mID = ID 1, mUser = testUser}
          let testUser2 = User 3 :: User
          let testMessage2 = Message {mData = Gif (T.pack textMessage), mID = ID 2, mUser = testUser2}

          let logHandle =
                Handlers.Logger.Handle
                  { Handlers.Logger.levelLogger = Debug,
                    Handlers.Logger.writeLog = \_ -> pure ()
                  } ::
                  Handlers.Logger.Handle (State T.Text)
          let baseHandle =
                Handlers.Base.Handle
                  { Handlers.Base.findUser = \_ -> pure $ Just (RepeatCount $ cRepeatCount testConfig),
                    Handlers.Base.logger = logHandle
                  }
          let clientHandle =
                Handlers.Client.Handle
                  { Handlers.Client.carryAway =
                      \(Message {mData = msg}) -> case msg of
                        Msg text -> void $ modify (<> text)
                        Gif text -> void $ modify (<> text)
                        _ -> pure (),
                    Handlers.Client.logger = logHandle
                  }
          let botHandle =
                Handlers.Bot.Handle
                  { Handlers.Bot.repeatMessage = cTextMenuRepeat testConfig,
                    Handlers.Bot.helpMessage = cTextMenuHelp testConfig,
                    Handlers.Bot.logger = logHandle,
                    Handlers.Bot.client = clientHandle,
                    Handlers.Bot.base = baseHandle
                  }

          execState (Handlers.Bot.makeReaction botHandle testMessage) ""
            `shouldBe` mconcat (replicate (cRepeatCount testConfig) (T.pack textMessage))
          execState (Handlers.Bot.makeReaction botHandle testMessage2) ""
            `shouldBe` mconcat (replicate (cRepeatCount testConfig) (T.pack textMessage))

      it "Get message for only one user" $ do
        property $ \testUser -> do
          let testText = "Bot logic"
          let testMessage = Message {mData = Msg testText, mID = ID 1, mUser = User testUser}
          let logerHandle =
                Handlers.Logger.Handle
                  { Handlers.Logger.levelLogger = Debug,
                    Handlers.Logger.writeLog = \_ -> pure ()
                  } ::
                  Handlers.Logger.Handle (State (Maybe Message, Maybe LastMessage))
          let baseHandle =
                Handlers.Base.Handle
                  { Handlers.Base.readStackMessage = pure (Just testMessage, Nothing),
                    Handlers.Base.eraseMessage = \_ -> pure (),
                    Handlers.Base.logger = logerHandle
                  }
          let clientHandle =
                Handlers.Client.Handle
                  { Handlers.Client.fetch = pure,
                    Handlers.Client.carryAway = \_ -> pure (),
                    Handlers.Client.logger = logerHandle
                  }
          let botHandle =
                Handlers.Bot.Handle
                  { Handlers.Bot.base = baseHandle,
                    Handlers.Bot.client = clientHandle,
                    Handlers.Bot.logger = logerHandle
                  }
          evalState (Handlers.Bot.getMessage botHandle (User testUser)) (Just testMessage, Nothing)
            `shouldBe` testMessage


testConfig :: Config
testConfig =
  Config
    { cRepeatCount = 3,
      cTextMenuHelp = "Help menu:" :: T.Text,
      cTextMenuRepeat = "Repeat menu:" :: T.Text,
      cLvlLog = Debug :: Log
    }
