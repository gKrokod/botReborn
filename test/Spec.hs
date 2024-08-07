import Control.Monad (void)
import Control.Monad.State (State (..), evalState, execState, get, modify, put)
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map (Map, fromList, insert, lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Handlers.Base (Handle (..), giveRepeatCountFromBase)
import qualified Handlers.Bot (Handle (..), changeRepeatCountForUser, isCorrectRepeatCount, makeReaction)
import qualified Handlers.Client (Handle (..))
import qualified Handlers.Dispatcher (Handle (..), dispatcher, getMessage, watcherForNewMessage)
import qualified Handlers.Logger (Handle (..), logMessage)
import Test.Hspec (context, describe, hspec, it, shouldBe, shouldNotBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)
import Text.Read (readMaybe)
import Types (Config (..), Data (..), LastMessage, Log (..), Message (..), RepeatCount, User)

main :: IO ()
main = hspec $ do
  describe "Base logic" $ do
    let testUser = 341 :: User
    let newUser = 11 :: User
    let repeatCount = 2 :: RepeatCount
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
              Handlers.Base.defaultRepeatCount = cRepeatCount testConfig,
              Handlers.Base.findUser = \user -> get >>= \base -> pure $ Map.lookup user base,
              Handlers.Base.updateUser = \user count -> void $ modify (Map.insert user count)
            }
    it "return repeat count from Base for user" $
      evalState (Handlers.Base.giveRepeatCountFromBase baseHandle testUser) testBase
        `shouldBe` repeatCount

    it "return default repeat count from Base for new user" $
      evalState (Handlers.Base.giveRepeatCountFromBase baseHandle newUser) testBase
        `shouldBe` cRepeatCount testConfig

  describe "Bot logic" $ modifyMaxSuccess (const 1000) $ do
    it "User can change his repeat count" $ do
      let oldRepeatCount = 1 :: RepeatCount
      let newRepeatCount = 2 :: RepeatCount
      let testUser = 111 :: User
      let testBase = Map.fromList [(testUser, oldRepeatCount)]

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
                Handlers.Base.logger = loggerHandle
              }
      let botHandle =
            Handlers.Bot.Handle
              { Handlers.Bot.sendMessage = \_ -> pure (),
                Handlers.Bot.getMessage = pure (Message {mData = Query newRepeatCount, mID = 1, mUser = testUser}),
                Handlers.Bot.repeatMessage = cTextMenuRepeat testConfig,
                Handlers.Bot.helpMessage = cTextMenuHelp testConfig,
                Handlers.Bot.logger = loggerHandle,
                Handlers.Bot.base = baseHandle
              }

      evalState (Handlers.Base.giveRepeatCountFromBase baseHandle testUser) testBase
        `shouldBe` oldRepeatCount
      evalState
        ( Handlers.Bot.changeRepeatCountForUser botHandle testUser
            >> Handlers.Base.giveRepeatCountFromBase baseHandle testUser
        )
        testBase
        `shouldBe` newRepeatCount

    context "User can change repeat count only in range [1..5]" $ do
      it "Input: Int" $ do
        property $ \answer -> do
          let msg = Message {mData = Query answer, mID = 1, mUser = 2}
          Handlers.Bot.isCorrectRepeatCount msg
            `shouldBe` 1 <= answer && answer <= 5

      it "Input: Text" $ do
        property $ \answer -> do
          let numberMessage =
                if length answer /= 1
                  then 0
                  else fromMaybe 0 (readMaybe answer :: Maybe Int)
          let msg = Message {mData = Msg (T.pack answer), mID = 1, mUser = 2}

          Handlers.Bot.isCorrectRepeatCount msg
            `shouldBe` 1 <= numberMessage && numberMessage <= 5

    context "Correct text in answer on:" $ do
      it "/help" $ do
        property $ \helpMessage -> do
          let testMessage = Message {mData = Command "/help", mID = 1, mUser = 2}

          let logHandle =
                Handlers.Logger.Handle
                  { Handlers.Logger.levelLogger = Debug,
                    Handlers.Logger.writeLog = \_ -> pure ()
                  } ::
                  Handlers.Logger.Handle (State T.Text)
          let botHandle =
                Handlers.Bot.Handle
                  { Handlers.Bot.sendMessage =
                      \(Message {mData = msg}) -> case msg of
                        Msg text -> void $ put text
                        _ -> pure (),
                    Handlers.Bot.helpMessage = T.pack helpMessage,
                    Handlers.Bot.logger = logHandle
                  }

          execState (Handlers.Bot.makeReaction botHandle testMessage) "no help menu"
            `shouldBe` T.pack helpMessage

      it "/repeat" $ do
        property $ \repeatMessage -> do
          let testUser = 2 :: User
          let testMessage = Message {mData = Command "/repeat", mID = 1, mUser = testUser}
          let repeatCount = 5 :: RepeatCount

          let logHandle =
                Handlers.Logger.Handle
                  { Handlers.Logger.levelLogger = Debug,
                    Handlers.Logger.writeLog = \_ -> pure ()
                  } ::
                  Handlers.Logger.Handle (State T.Text)
          let baseHandle =
                Handlers.Base.Handle
                  { Handlers.Base.findUser = \_ -> pure $ Just repeatCount,
                    Handlers.Base.updateUser = \_ _ -> pure ()
                  }
          let botHandle =
                Handlers.Bot.Handle
                  { Handlers.Bot.sendMessage =
                      \(Message {mData = msg}) -> case msg of
                        Msg text -> void $ put text
                        _ -> pure (),
                    Handlers.Bot.repeatMessage = T.pack repeatMessage,
                    Handlers.Bot.getMessage = pure (Message {mData = Msg "1", mID = 1, mUser = testUser}),
                    Handlers.Bot.logger = logHandle,
                    Handlers.Bot.base = baseHandle
                  }
          execState (Handlers.Bot.makeReaction botHandle testMessage) "no repeat menu"
            `shouldBe` T.pack (repeatMessage <> show repeatCount)

      it "text message and gif message" $ do
        property $ \textMessage -> do
          let testUser = 2 :: User
          let testMessage = Message {mData = Msg (T.pack textMessage), mID = 1, mUser = testUser}
          let testUser2 = 3 :: User
          let testMessage2 = Message {mData = Gif (T.pack textMessage), mID = 2, mUser = testUser2}

          let logHandle =
                Handlers.Logger.Handle
                  { Handlers.Logger.levelLogger = Debug,
                    Handlers.Logger.writeLog = \_ -> pure ()
                  } ::
                  Handlers.Logger.Handle (State T.Text)
          let baseHandle =
                Handlers.Base.Handle
                  { Handlers.Base.findUser = \_ -> pure $ Just (cRepeatCount testConfig),
                    Handlers.Base.logger = logHandle
                  }
          let botHandle =
                Handlers.Bot.Handle
                  { Handlers.Bot.sendMessage =
                      \(Message {mData = msg}) -> case msg of
                        Msg text -> void $ modify (<> text)
                        Gif text -> void $ modify (<> text)
                        _ -> pure (),
                    Handlers.Bot.repeatMessage = cTextMenuRepeat testConfig,
                    Handlers.Bot.helpMessage = cTextMenuHelp testConfig,
                    Handlers.Bot.logger = logHandle,
                    Handlers.Bot.base = baseHandle
                  }

          execState (Handlers.Bot.makeReaction botHandle testMessage) ""
            `shouldBe` mconcat
            $ replicate (cRepeatCount testConfig) (T.pack textMessage)
          execState (Handlers.Bot.makeReaction botHandle testMessage2) ""
            `shouldBe` mconcat
            $ replicate (cRepeatCount testConfig) (T.pack textMessage)

  describe "Client logic" $ do
    it "No logic, no test" $ do
      Debug
        `shouldBe` Debug

  describe "Dispatcher logic" $ do
    it "Get message for only one user" $ do
      property $ \testUser -> do
        let testText = "Disptacher logic"
        let testMessage = Message {mData = Msg testText, mID = 1, mUser = testUser}
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
        let botHandle =
              Handlers.Bot.Handle
                { Handlers.Bot.base = baseHandle,
                  Handlers.Bot.logger = logerHandle
                }
        let clientHandle =
              Handlers.Client.Handle
                { Handlers.Client.fetch = pure,
                  Handlers.Client.carryAway = \_ -> pure (),
                  Handlers.Client.logger = logerHandle
                }
        let dispatcherHandle =
              Handlers.Dispatcher.Handle
                { Handlers.Dispatcher.client = clientHandle,
                  Handlers.Dispatcher.bot = botHandle,
                  Handlers.Dispatcher.logger = logerHandle
                }
        evalState (Handlers.Dispatcher.getMessage dispatcherHandle testUser) (Just testMessage, Nothing)
          `shouldBe` testMessage

    it "Take message from client if stackMessages haven't a new message" $ do
      let testUser = 341 :: User
      let testText = "some message"
      let testMessage = Message {mData = Msg testText, mID = 1, mUser = testUser}
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
      let botHandle =
            Handlers.Bot.Handle
              { Handlers.Bot.base = baseHandle,
                Handlers.Bot.logger = logerHandle
              }
      let clientHandle =
            Handlers.Client.Handle
              { Handlers.Client.fetch = \_ -> pure $ Just testMessage,
                Handlers.Client.carryAway = \_ -> pure (),
                Handlers.Client.logger = logerHandle
              }
      let dispatcherHandle =
            Handlers.Dispatcher.Handle
              { Handlers.Dispatcher.client = clientHandle,
                Handlers.Dispatcher.bot = botHandle,
                Handlers.Dispatcher.logger = logerHandle
              }
      evalState
        ( Handlers.Dispatcher.watcherForNewMessage dispatcherHandle
            >> Handlers.Dispatcher.getMessage dispatcherHandle testUser
        )
        testStack
        `shouldBe` testMessage

    it "Save a new user in Base and start fork for him because of catch his message" $ do
      let testUser = 341 :: User
      let newUser = 444 :: User
      let repeatCount = 5 :: RepeatCount
      let forkCount = 111 :: RepeatCount
      let testBase = Map.fromList [(testUser, repeatCount)]
      let testBase2 = Map.fromList [(testUser, repeatCount), (newUser, repeatCount)]
      let testText = "some message"
      let testMessage = Message {mData = Msg testText, mID = 1, mUser = newUser}
      let testStack = (Just testMessage, Nothing) :: (Maybe Message, Maybe LastMessage)
      let testForkImitation = 11111111 :: User
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
                Handlers.Base.defaultRepeatCount = cRepeatCount testConfig,
                Handlers.Base.findUser = \user -> get >>= \base -> pure $ Map.lookup user base,
                Handlers.Base.updateUser = \user count -> void $ modify (Map.insert user count)
              }
      let botHandle =
            Handlers.Bot.Handle
              { Handlers.Bot.base = baseHandle,
                Handlers.Bot.logger = logerHandle
              }

      let clientHandle =
            Handlers.Client.Handle
              { Handlers.Client.logger = logerHandle
              }
      let dispatcherHandle =
            Handlers.Dispatcher.Handle
              { Handlers.Dispatcher.client = clientHandle,
                Handlers.Dispatcher.bot = botHandle,
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
        `shouldBe` Just (cRepeatCount testConfig)

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
    -- describe "new" $ do
    -- context "Logger write log message if level log message >=  level from Config:" $ do
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
    { cRepeatCount = 3 :: RepeatCount,
      cTextMenuHelp = "Help menu:" :: T.Text,
      cTextMenuRepeat = "Repeat menu:" :: T.Text,
      cLvlLog = Debug :: Log
    }
