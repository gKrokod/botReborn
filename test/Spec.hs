import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import Text.Read (readMaybe)
import Types
import Control.Monad.Identity
import Control.Monad.State
import Data.Char (isDigit)
import qualified Handlers.Logger
import qualified Handlers.Base
import qualified Handlers.Bot
import qualified Handlers.Client
import qualified Handlers.Dispatcher

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map



main :: IO ()
main = hspec $ do
  describe "Base logic" $ do
    let baseHandle = baseHandleI testConfig
    it "returns the default repeat count from Config for new users" $
      property $ \user -> runIdentity (Handlers.Base.giveRepeatCountFromBase baseHandle user)
                            `shouldBe` cRepeatCount testConfig

    it "returns default repeat counts from Config for the new user" $
      property $ \count -> do
        let baseHandle = baseHandleI $ testConfig {cRepeatCount = count}
        runIdentity (Handlers.Base.giveRepeatCountFromBase baseHandle (1 :: User)) 
	  `shouldBe` count

  describe "Bot logic" $ modifyMaxSuccess (const 10) $ do
    it "User can change his repeat count" $ do
      let oldRepeatCount = 4 :: RepeatCount
      let newRepeatCount = 5 :: RepeatCount
      let testUser = 111 :: User
      let testBase = Map.fromList [(testUser,oldRepeatCount)]

      let loggerHandle = Handlers.Logger.Handle
                     { Handlers.Logger.levelLogger = Debug
                     , Handlers.Logger.writeLog = \_ -> pure ()
                     } :: Handlers.Logger.Handle (State (Map.Map User RepeatCount))
      let baseHandle = Handlers.Base.Handle
	              {  Handlers.Base.findUser = \user -> get >>= \base -> pure $ Map.lookup user base
	              ,  Handlers.Base.updateUser = \user count -> modify (Map.insert user count) >> pure () 
	              ,  Handlers.Base.logger = loggerHandle
	              }
      let botHandle = Handlers.Bot.Handle
	             { Handlers.Bot.sendMessage = \_ -> pure () 
		     , Handlers.Bot.getMessage = pure (Message {mData = Query newRepeatCount, mID = 1, mUser = testUser})
		     , Handlers.Bot.repeatMessage = cTextMenuRepeat testConfig
	             , Handlers.Bot.helpMessage = cTextMenuHelp testConfig
	             , Handlers.Bot.logger = loggerHandle
		     , Handlers.Bot.base = baseHandle
	             }
      
      evalState (Handlers.Base.giveRepeatCountFromBase baseHandle testUser ) testBase 
        `shouldBe` oldRepeatCount
      evalState (Handlers.Bot.changeRepeatCountForUser botHandle testUser >>
		 Handlers.Base.giveRepeatCountFromBase baseHandle testUser ) testBase 
        `shouldBe` newRepeatCount

    context "User can change repeat count only in range [1..5]" $ do
      it "Input: Int" $ do
	property $ \answer -> do
	  let msg = Message {mData = Query answer, mID = 1, mUser = 2}
	  Handlers.Bot.isCorrectRepeatCount msg 
	    `shouldBe` 1 <= answer && answer <= 5

      it "Input: Text" $ do
	property $ \answer -> do
	  let numberMessage = if length answer /= 1 then 0
	                      else maybe 0 id (readMaybe answer :: Maybe Int)
	  let msg = Message {mData = Msg (T.pack answer), mID = 1, mUser = 2}	 
	  Handlers.Bot.isCorrectRepeatCount msg 
	    `shouldBe` 1 <= numberMessage && numberMessage <= 5
    
    context "Correct text in answer on:" $ do
      it "/help" $ do
        property $ \helpMessage -> do
  	  let logHandle = Handlers.Logger.Handle
	       { Handlers.Logger.levelLogger = Debug
	       , Handlers.Logger.writeLog = \_ -> pure ()
	       } :: Handlers.Logger.Handle (State T.Text)
          let botHandle = Handlers.Bot.Handle
		{ Handlers.Bot.sendMessage = \(Message {mData = msg}) -> case msg of
									    Msg text -> put text >> pure ()
									    _ -> pure ()
		, Handlers.Bot.helpMessage = T.pack helpMessage
		, Handlers.Bot.logger = logHandle
		}
	  let testMessage = Message {mData = Command "/help", mID = 1, mUser = 2}
          execState (Handlers.Bot.makeReaction botHandle testMessage) "no help menu"
	    `shouldBe` T.pack helpMessage

      it "/repeat" $ do
        property $ \repeatMessage -> do
	  let testUser = 2 :: User
	  let testMessage = Message {mData = Command "/repeat", mID = 1, mUser = testUser}
	  let repeatCount = 5 :: RepeatCount
	  
  	  let logHandle = Handlers.Logger.Handle
	       { Handlers.Logger.levelLogger = Debug
	       , Handlers.Logger.writeLog = \_ -> pure ()
	       } :: Handlers.Logger.Handle (State T.Text)
          let baseHandle = Handlers.Base.Handle
	        {  Handlers.Base.findUser = \user -> pure $ Just repeatCount
	        ,  Handlers.Base.updateUser = \user repeatCount -> pure ()
	        }
          let botHandle = Handlers.Bot.Handle
		{ Handlers.Bot.sendMessage = \(Message {mData = msg}) -> case msg of
									    Msg text -> put text >> pure ()
									    _ -> pure ()
		, Handlers.Bot.repeatMessage = T.pack repeatMessage
		, Handlers.Bot.getMessage = pure (Message {mData = Msg "1", mID = 1, mUser = testUser})
		, Handlers.Bot.logger = logHandle
		, Handlers.Bot.base = baseHandle
		}
          execState (Handlers.Bot.makeReaction botHandle testMessage) "no repeat menu" 
	    `shouldBe` (T.pack (repeatMessage <> show repeatCount))

      it "text message and gif message" $ do
        property $ \textMessage -> do
	  let testUser = 2 :: User
	  let testMessage = Message {mData = Msg (T.pack textMessage), mID = 1, mUser = testUser}
	  let testUser2 = 3 :: User
	  let testMessage2 = Message {mData = Gif (T.pack textMessage), mID = 2, mUser = testUser2}

  	  let logHandle = Handlers.Logger.Handle
	       { Handlers.Logger.levelLogger = Debug
	       , Handlers.Logger.writeLog = \_ -> pure ()
	       } :: Handlers.Logger.Handle (State T.Text)
          let baseHandle = Handlers.Base.Handle
	        {  Handlers.Base.findUser = \user -> pure $ Just (cRepeatCount testConfig)
	        ,  Handlers.Base.logger = logHandle
	        }
          let botHandle = Handlers.Bot.Handle
		{ Handlers.Bot.sendMessage = \(Message {mData = msg}) -> case msg of
									    Msg text -> modify (<> text) >> pure ()
									    Gif text -> modify (<> text) >> pure ()
									    _ -> pure ()
		, Handlers.Bot.repeatMessage = cTextMenuRepeat testConfig
		, Handlers.Bot.helpMessage = cTextMenuHelp testConfig
		, Handlers.Bot.logger = logHandle
		, Handlers.Bot.base = baseHandle
		}
	  
          execState (Handlers.Bot.makeReaction botHandle testMessage) ""
	    `shouldBe` (mconcat $ replicate (cRepeatCount testConfig) (T.pack textMessage))
          execState (Handlers.Bot.makeReaction botHandle testMessage2) ""
	    `shouldBe` (mconcat $ replicate (cRepeatCount testConfig) (T.pack textMessage))
    
      
-- dispatcher :: (Monad m) => Handle m -> m ()
-- dispatcher h = do
--   let logHandle = logger h
--   let botHandle = bot h
--   let baseHandle = Handlers.Bot.base botHandle
--   (stack, lastMsg) <- Handlers.Base.readStackMessage baseHandle
--   -- HL.logMessage logHandle Debug "ReadStackMessage from Dispatcher"
--   case stack of
--     Nothing -> do 
--       -- HL.logMessage logHandle Debug "Нет новых сообщений для обработки"
--       pure ()
--     Just msg -> do
--       let user = mUser msg
--       existUser <- Handlers.Base.findUser baseHandle user
--       case existUser of
--         Just _ -> pure ()
--         Nothing -> do
-- 	 -- если в базе пользователя нет, то сохрани его в базе с дефолтными настройкам и создай для него поток
--           HL.logMessage logHandle Debug (mconcat ["Пользователь ", T.pack $ show user," не найден"])
--           Handlers.Base.updateUser baseHandle user (Handlers.Base.defaultRepeatCount baseHandle)
--           HL.logMessage logHandle Debug (mconcat ["Пользователь ", T.pack $ show user," сохранен в базе (диспетчер)"])
-- 	  forkForUser h 
-- 	    (Handlers.Bot.doWork (botHandle {Handlers.Bot.getMessage = getMessage h user}))
--           HL.logMessage logHandle Debug (mconcat ["Запустили новый поток для пользователя: ", T.pack $ show user])
-- 	 
-- data Handle m = Handle 
--   {  defaultRepeatCount :: RepeatCount
--   ,  readStackMessage :: m (Maybe Message, Maybe LastMessage)
--   ,  saveMessage :: Message -> m ()
--   ,  eraseMessage :: Message -> m ()
--   ,  findUser :: User -> m (Maybe RepeatCount)
--   ,  updateUser :: User -> RepeatCount -> m ()
--   ,  logger :: HL.Handle m  
--   }
-- data Handle m = Handle
--   {  getMessage :: m (Message)  = pure (Message {mData = Query 5, mID = 1, mUser = 2}
--   {  getMessage :: m (Message)  = pure (Message {mData = Msg 1, mID = 1, mUser = 2}
--   ,  sendMessage :: Message -> m () = \_ -> pure ()
--   ,  base :: Handlers.Base.Handle m
--   ,  helpMessage :: T.Text
--   ,  repeatMessage :: T.Text
--   ,  logger :: HL.Handle m  
--   }
-- data Handle m = Handle
--   { client :: Handlers.Client.Handle m
--   , bot :: Handlers.Bot.Handle m
--   , logger :: HL.Handle m  
--   , forkForUser :: m () -> m ()
--   }
  describe "Client logic" $ do
    it "No logic, no test" $ do
      Debug `shouldBe` Debug
  
  describe "Dispatcher logic" $ do
    it "Get message for only one user" $ do
      property $ \testUser -> do
        let testText = "Disptacher logic"
	let testMessage = Message {mData = Msg testText, mID = 1, mUser = testUser}

	let logerHandle = Handlers.Logger.Handle
		       { Handlers.Logger.levelLogger = Debug
		       , Handlers.Logger.writeLog = \_ -> pure ()
		       } :: Handlers.Logger.Handle (State (Maybe Message, Maybe LastMessage))
	let baseHandle = Handlers.Base.Handle
			{  Handlers.Base.readStackMessage = pure (Just testMessage, Nothing)
			,  Handlers.Base.eraseMessage = \message -> pure () 
			,  Handlers.Base.logger = logerHandle
			}
	let botHandle = Handlers.Bot.Handle
		       { Handlers.Bot.base = baseHandle
		       , Handlers.Bot.logger = logerHandle
		       }
	let clientHandle = Handlers.Client.Handle
			{  Handlers.Client.fetch = \message -> pure message
			,  Handlers.Client.carryAway = \message -> pure ()
			,  Handlers.Client.logger = logerHandle
			}
	let dispatcherHandle = Handlers.Dispatcher.Handle
		       { Handlers.Dispatcher.client = clientHandle
		       , Handlers.Dispatcher.bot = botHandle
		       , Handlers.Dispatcher.logger = logerHandle
		       }
        evalState (Handlers.Dispatcher.getMessage dispatcherHandle testUser) (Just testMessage, Nothing)
	  `shouldBe` testMessage

    it "Take message from client if stackMessages haven't message" $ do
        let testUser = 341 :: User 
	let testText = "some message"
	let testMessage = Message {mData = Msg testText, mID = 1, mUser = testUser}
	let testStack = (Nothing, Nothing) :: (Maybe Message, Maybe LastMessage)

	let logerHandle = Handlers.Logger.Handle
		       { Handlers.Logger.levelLogger = Debug
		       , Handlers.Logger.writeLog = \_ -> pure ()
		       } :: Handlers.Logger.Handle (State (Maybe Message, Maybe LastMessage))
	let baseHandle = Handlers.Base.Handle
			{  Handlers.Base.readStackMessage = get >>= pure
			,  Handlers.Base.saveMessage = \message -> put (Just message, Nothing) >> pure ()
			,  Handlers.Base.logger = logerHandle
			}
	let botHandle = Handlers.Bot.Handle
		       { Handlers.Bot.base = baseHandle
		       , Handlers.Bot.logger = logerHandle
		       }
	let clientHandle = Handlers.Client.Handle
			{  Handlers.Client.fetch = \lastMessage -> pure $ Just testMessage 
			,  Handlers.Client.carryAway = \message -> pure ()
			,  Handlers.Client.logger = logerHandle
			}
	let dispatcherHandle = Handlers.Dispatcher.Handle
		       { Handlers.Dispatcher.client = clientHandle
		       , Handlers.Dispatcher.bot = botHandle
		       , Handlers.Dispatcher.logger = logerHandle
		       }
        evalState (Handlers.Dispatcher.watcherForNewMessage dispatcherHandle >>
                   Handlers.Dispatcher.getMessage dispatcherHandle testUser) testStack
	  `shouldBe` testMessage

    it "Save new user in Base" $ do
        let testUser = 341 :: User 
	let testText = "some message"
	let testMessage = Message {mData = Msg testText, mID = 1, mUser = testUser}
	let testStack = (Nothing, Nothing) :: (Maybe Message, Maybe LastMessage)

	let logerHandle = Handlers.Logger.Handle
		       { Handlers.Logger.levelLogger = Debug
		       , Handlers.Logger.writeLog = \_ -> pure ()
		       } :: Handlers.Logger.Handle (State (Maybe Message, Maybe LastMessage))
	let baseHandle = Handlers.Base.Handle
			{  Handlers.Base.readStackMessage = get >>= pure
			,  Handlers.Base.saveMessage = \message -> put (Just message, Nothing) >> pure ()
			,  Handlers.Base.logger = logerHandle
			}
	let botHandle = Handlers.Bot.Handle
		       { Handlers.Bot.base = baseHandle
		       , Handlers.Bot.logger = logerHandle
		       }
	let clientHandle = Handlers.Client.Handle
			{  Handlers.Client.fetch = \lastMessage -> pure $ Just testMessage 
			,  Handlers.Client.carryAway = \message -> pure ()
			,  Handlers.Client.logger = logerHandle
			}
	let dispatcherHandle = Handlers.Dispatcher.Handle
		       { Handlers.Dispatcher.client = clientHandle
		       , Handlers.Dispatcher.bot = botHandle
		       , Handlers.Dispatcher.logger = logerHandle
		       }
        evalState (Handlers.Dispatcher.watcherForNewMessage dispatcherHandle >>
                   Handlers.Dispatcher.getMessage dispatcherHandle testUser) testStack
	  `shouldBe` testMessage
-- dispatcher :: (Monad m) => Handle m -> m ()
-- dispatcher h = do
--   let logHandle = logger h
--   let botHandle = bot h
--   let baseHandle = Handlers.Bot.base botHandle
--   (stack, lastMsg) <- Handlers.Base.readStackMessage baseHandle
--   -- HL.logMessage logHandle Debug "ReadStackMessage from Dispatcher"
--   case stack of
--     Nothing -> do 
--       -- HL.logMessage logHandle Debug "Нет новых сообщений для обработки"
--       pure ()
--     Just msg -> do
--       let user = mUser msg
--       existUser <- Handlers.Base.findUser baseHandle user
--       case existUser of
--         Just _ -> pure ()
--         Nothing -> do
-- 	 -- если в базе пользователя нет, то сохрани его в базе с дефолтными настройкам и создай для него поток
--           HL.logMessage logHandle Debug (mconcat ["Пользователь ", T.pack $ show user," не найден"])
--           Handlers.Base.updateUser baseHandle user (Handlers.Base.defaultRepeatCount baseHandle)
--           HL.logMessage logHandle Debug (mconcat ["Пользователь ", T.pack $ show user," сохранен в базе (диспетчер)"])
-- 	  forkForUser h 
-- 	    (Handlers.Bot.doWork (botHandle {Handlers.Bot.getMessage = getMessage h user}))
--           HL.logMessage logHandle Debug (mconcat ["Запустили новый поток для пользователя: ", T.pack $ show user])
-- 	 
-- watcherForNewMessage :: (Monad m) => Handle m -> m ()
-- watcherForNewMessage h = do
--   let logHandle = logger h
--   let baseHandle = Handlers.Bot.base (bot h)
--   let clientHandle = client h
--   (stack, lastMsg) <- Handlers.Base.readStackMessage baseHandle
--   case stack of
--     Just _ -> pure ()
--     Nothing -> do
--       HL.logMessage logHandle Debug "Нет новых необработанных сообщений от клиента, начинаем постоянный запрос"
--       loop
--       where loop = do
-- 	      fetchedMessage <- Handlers.Client.fetch clientHandle lastMsg
-- 	      case fetchedMessage of
-- 	        Nothing -> loop
-- 	        Just msg -> Handlers.Base.saveMessage baseHandle msg
-- getMessage :: (Monad m) => Handle m -> User -> m (Message)
-- getMessage h user = do
--   let logHandle = logger h
--   (stack, lastMsg) <- Handlers.Base.readStackMessage (Handlers.Bot.base $ bot h)
--   case stack of
--     Just msg -> if mUser msg == user
--                 then do
-- 		  Handlers.Base.eraseMessage (Handlers.Bot.base $ bot h) msg
--                   HL.logMessage logHandle Debug (mconcat ["Получено сообщение для пользователя ", T.pack $ show user, " из базы данных"])
-- 		  pure msg
--                 else getMessage h user
--     Nothing -> getMessage h user 
  describe "Logger logic" $ do -- describe "new" $ do 
    -- context "Logger write log message if level log message >=  level from Config:" $ do
    it "Logger write log message if level log message >=  level from Config" $ do
      let logHandle' = logHandleS testConfig
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
      let logHandle' = logHandleS testConfig
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
testConfig = Config
	     {  cRepeatCount = 3 :: RepeatCount
	     ,  cTextMenuHelp = "Help menu:" :: T.Text 
	     ,  cTextMenuRepeat = "Repeat menu:" :: T.Text
	     ,  cLvlLog = Debug :: Log
	     } 

logHandleI :: Config -> Handlers.Logger.Handle Identity
logHandleI cfg = Handlers.Logger.Handle
	       { Handlers.Logger.levelLogger = cLvlLog cfg
	       , Handlers.Logger.writeLog = \text -> pure () 
	       }

logHandleS :: Config -> Handlers.Logger.Handle (State T.Text)
logHandleS cfg = Handlers.Logger.Handle
	       { Handlers.Logger.levelLogger = cLvlLog cfg
	       , Handlers.Logger.writeLog = \text -> put text >> pure ()
	       }

baseHandleI :: Config -> Handlers.Base.Handle Identity
baseHandleI cfg = Handlers.Base.Handle
		 {  Handlers.Base.defaultRepeatCount = cRepeatCount cfg
		 ,  Handlers.Base.readStackMessage = pure (Nothing, Nothing)
		 ,  Handlers.Base.saveMessage = \_ -> pure ()
		 ,  Handlers.Base.eraseMessage = \_ -> pure ()
		 ,  Handlers.Base.findUser = \user -> pure (Just $ cRepeatCount cfg)
		 ,  Handlers.Base.updateUser = \user count -> pure ()
		 ,  Handlers.Base.logger = logHandleI cfg
		 }

botHandleI :: Config -> Handlers.Bot.Handle Identity
botHandleI cfg = Handlers.Bot.Handle 
		{ Handlers.Bot.getMessage = pure (Message {mData = Msg ("Test message"), mID = 1, mUser = 2})
		, Handlers.Bot.sendMessage = \text -> pure ()
		, Handlers.Bot.base = baseHandleI cfg
		, Handlers.Bot.helpMessage = cTextMenuHelp cfg
		, Handlers.Bot.repeatMessage = cTextMenuRepeat cfg
		, Handlers.Bot.logger = logHandleI cfg
		}

