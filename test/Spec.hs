import Config (Config (..))
import Control.Monad (void)
import Control.Monad.State (State (..), evalState, execState, get, modify, put)
import qualified Data.Map.Strict as Map (Map, fromList, insert, lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Handlers.Base (Handle (..), giveRepeatCountFromBase)
import qualified Handlers.Bot (Handle (..), changeRepeatCountForUser, getMessage, isCorrectRepeatCount, makeReaction)
import qualified Handlers.Client (Handle (..))
import qualified Handlers.Dispatcher (Handle (..), dispatcher, watcherForNewMessage)
import qualified Handlers.Logger (Handle (..), logMessage)
import Test.Hspec (context, describe, hspec, it, shouldBe, shouldNotBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)
import Text.Read (readMaybe)
import Types (Command (..), Data (..), DataFromButton (..), ID (..), LastMessage, Log (..), Message (..), RepeatCount (..), User (..))

main :: IO ()
main = hspec $ do
  describe "Base logic" $ do
    let testUser = User 341 :: User
    let newUser = User 11 :: User
    let repeatCount = RepeatCount 2 :: RepeatCount
    let testBase = Map.fromList [(testUser, repeatCount)]
    let logerHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            } ::
            Handlers.Logger.Handle (State (Map.Map User RepeatCount))
    let baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logerHandle,
              Handlers.Base.defaultRepeatCount = RepeatCount $ cRepeatCount testConfig,
              Handlers.Base.findUser = \user -> get >>= \base -> pure $ Map.lookup user base,
              Handlers.Base.updateUser = \user count -> void $ modify (Map.insert user count)
            }
    it "return repeat count from Base for user" $
      evalState (Handlers.Base.giveRepeatCountFromBase baseHandle testUser) testBase
        `shouldBe` repeatCount

    it "return default repeat count from Base for new user" $
      evalState (Handlers.Base.giveRepeatCountFromBase baseHandle newUser) testBase
        `shouldBe` RepeatCount (cRepeatCount testConfig)

  describe "Bot logic" $ modifyMaxSuccess (const 1000) $ do
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
          let dispatcherHandle =
                Handlers.Dispatcher.Handle
                  { Handlers.Dispatcher.bot = botHandle,
                    Handlers.Dispatcher.logger = logerHandle
                  }
          evalState (Handlers.Bot.getMessage botHandle (User testUser)) (Just testMessage, Nothing)
            `shouldBe` testMessage

  describe "Client logic" $ do
    it "No logic, no test" $ do
      Debug
        `shouldBe` Debug

  describe "Dispatcher logic" $ do
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

  describe "Logger logic" $ do
    it "Logger write log message if level log message >=  level from Config" $ do
      let logHandle' =
            Handlers.Logger.Handle
              { Handlers.Logger.levelLogger = cLvlLog testConfig,
                Handlers.Logger.writeLog = void . put
              } ::
              Handlers.Logger.Handle (State T.Text)
      let logHandle = logHandle' {Handlers.Logger.levelLogger = Debug}
      -- it "Debug    vs  Debug" $ do
      execState (Handlers.Logger.logMessage logHandle Debug "New log") "Old log"
        `shouldBe` "[Debug] New log"

      -- it "Waring   vs  Debug" $ do
      execState (Handlers.Logger.logMessage logHandle Warning "New log") "Old log"
        `shouldBe` "[Warning] New log"
      -- it "Error    vs  Debug" $ do
      execState (Handlers.Logger.logMessage logHandle Error "New log") "Old log"
        `shouldBe` "[Error] New log"
      -- it "Fatal    vs  Debug" $ do
      execState (Handlers.Logger.logMessage logHandle Fatal "New log") "Old log"
        `shouldBe` "[Fatal] New log"
      let logHandle = logHandle' {Handlers.Logger.levelLogger = Warning}
      -- it "Warning  vs  Warning" $ do
      execState (Handlers.Logger.logMessage logHandle Warning "New log") "Old log"
        `shouldBe` "[Warning] New log"
      -- it "Error    vs  Warning" $ do
      execState (Handlers.Logger.logMessage logHandle Error "New log") "Old log"
        `shouldBe` "[Error] New log"
      -- it "Fatal    vs  Warning" $ do
      execState (Handlers.Logger.logMessage logHandle Fatal "New log") "Old log"
        `shouldBe` "[Fatal] New log"
      let logHandle = logHandle' {Handlers.Logger.levelLogger = Error}
      -- it "Error    vs  Error" $ do
      execState (Handlers.Logger.logMessage logHandle Error "New log") "Old log"
        `shouldBe` "[Error] New log"
      -- it "Fatal    vs  Error" $ do
      execState (Handlers.Logger.logMessage logHandle Fatal "New log") "Old log"
        `shouldBe` "[Fatal] New log"
      let logHandle = logHandle' {Handlers.Logger.levelLogger = Fatal}
      -- it "Fatal    vs  Fatal" $ do
      execState (Handlers.Logger.logMessage logHandle Fatal "New log") "Old log"
        `shouldBe` "[Fatal] New log"

    it "Logger don't write log message if level log message < level from Config" $ do
      let logHandle' =
            Handlers.Logger.Handle
              { Handlers.Logger.levelLogger = cLvlLog testConfig,
                Handlers.Logger.writeLog = void . put
              } ::
              Handlers.Logger.Handle (State T.Text)
      let logHandle = logHandle' {Handlers.Logger.levelLogger = Warning}
      -- it "Debug    vs  Warning" $ do
      execState (Handlers.Logger.logMessage logHandle Debug "New log") "Old log"
        `shouldNotBe` "[Debug] New log"
      let logHandle = logHandle' {Handlers.Logger.levelLogger = Error}
      -- it "Debug    vs  Error" $ do
      execState (Handlers.Logger.logMessage logHandle Debug "New log") "Old log"
        `shouldNotBe` "[Debug] New log"
      -- it "Warning  vs  Error" $ do
      execState (Handlers.Logger.logMessage logHandle Warning "New log") "Old log"
        `shouldNotBe` "[Warning] New log"
      let logHandle = logHandle' {Handlers.Logger.levelLogger = Fatal}
      -- it "Debug    vs  Fatal" $ do
      execState (Handlers.Logger.logMessage logHandle Debug "New log") "Old log"
        `shouldNotBe` "[Debug] New log"
      -- it "Warning  vs  Fatal" $ do
      execState (Handlers.Logger.logMessage logHandle Warning "New log") "Old log"
        `shouldNotBe` "[Warning] New log"
      -- it "Error    vs  Fatal" $ do
      execState (Handlers.Logger.logMessage logHandle Error "New log") "Old log"
        `shouldNotBe` "[Error] New log"

testConfig :: Config
testConfig =
  Config
    { cRepeatCount = 3,
      cTextMenuHelp = "Help menu:" :: T.Text,
      cTextMenuRepeat = "Repeat menu:" :: T.Text,
      cLvlLog = Debug :: Log
    }
