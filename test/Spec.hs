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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO



main :: IO ()
main = hspec $ do
  describe "Base logic" $ do
    let bHandle = baseHandleI testConfig
    it "returns the default repeat count from Config for new users" $
      property $ \user -> runIdentity (Handlers.Base.giveRepeatCountFromBase bHandle user) == cRepeatCount testConfig

    it "returns default repeat counts from Config for the new user" $
      property $ \count -> do
        let bHandle = baseHandleI (testConfig {cRepeatCount = count})
        runIdentity (Handlers.Base.giveRepeatCountFromBase bHandle (1 :: User)) == count

  describe "Bot logic" $ modifyMaxSuccess (const 10) $ do
    context "User can change repeat counts only in range [1..5]" $ do
      it "Input: Int" $ do
	property $ \answer -> do
	  let msg = Message {mData = Query answer, mID = 1, mUser = 2}
	  Handlers.Bot.isCorrectRepeatCount msg == (1 <= answer && answer <= 5)

      it "Input: Text" $ do
	property $ \answer -> do
	  let numberMessage = if length answer /= 1 then 0
	                      else maybe 0 id (readMaybe answer :: Maybe Int)
	  let msg = Message {mData = Msg (T.pack answer), mID = 1, mUser = 2}	 
	  Handlers.Bot.isCorrectRepeatCount msg == (1 <= numberMessage && (numberMessage <= 5))
    
    context "Correct text in answer on:" $ do
      it "/help" $ do
        property $ \helpMessage -> do
  	  let lHandleS = Handlers.Logger.Handle
	       { Handlers.Logger.levelLogger = Debug
	       , Handlers.Logger.writeLog = \_ -> pure ()
	       } :: Handlers.Logger.Handle (State T.Text)
          let bHandleS = Handlers.Bot.Handle
		{ Handlers.Bot.sendMessage = \(Message {mData = msg}) -> case msg of
									    Msg text -> put text >> pure ()
									    _ -> pure ()
		, Handlers.Bot.helpMessage = T.pack helpMessage
		, Handlers.Bot.logger = lHandleS
		}
	  let testMessage = Message {mData = Command "/help", mID = 1, mUser = 2}
          execState (Handlers.Bot.makeReaction bHandleS testMessage) "no help menu" `shouldBe` (T.pack helpMessage)

      it "/repeat" $ do
        property $ \repeatMessage -> do
  	  let lHandleS = Handlers.Logger.Handle
	       { Handlers.Logger.levelLogger = Debug
	       , Handlers.Logger.writeLog = \_ -> pure ()
	       } :: Handlers.Logger.Handle (State T.Text)
          let bsHandleS = Handlers.Base.Handle
	        {  Handlers.Base.findUser = \user -> pure $ Just (5 :: RepeatCount)
	        ,  Handlers.Base.updateUser = \user repeatCount -> pure ()
	        }
          let bHandleS = Handlers.Bot.Handle
		{ Handlers.Bot.sendMessage = \(Message {mData = msg}) -> case msg of
									    Msg text -> put text >> pure ()
									    _ -> pure ()
		, Handlers.Bot.repeatMessage = T.pack repeatMessage
		, Handlers.Bot.getMessage = pure (Message {mData = Msg "1", mID = 1, mUser = 2})
		, Handlers.Bot.logger = lHandleS
		, Handlers.Bot.base = bsHandleS
		}
	  let testMessage = Message {mData = Command "/repeat", mID = 1, mUser = 2}
          execState (Handlers.Bot.makeReaction bHandleS testMessage) "no repeat menu" `shouldBe` (T.pack repeatMessage <> "5")

      it "text message and gif message" $ do
        property $ \textMessage -> do
  	  let lHandleS = Handlers.Logger.Handle
	       { Handlers.Logger.levelLogger = Debug
	       , Handlers.Logger.writeLog = \_ -> pure ()
	       } :: Handlers.Logger.Handle (State T.Text)
          let bsHandleS = Handlers.Base.Handle
	        {  Handlers.Base.findUser = \user -> pure $ Just (cRepeatCount testConfig)
	        ,  Handlers.Base.logger = lHandleS
	        }
          let bHandleS = Handlers.Bot.Handle
		{ Handlers.Bot.sendMessage = \(Message {mData = msg}) -> case msg of
									    Msg text -> modify (<> text) >> pure ()
									    Gif text -> modify (<> text) >> pure ()
									    _ -> pure ()
		, Handlers.Bot.repeatMessage = cTextMenuRepeat testConfig
		, Handlers.Bot.helpMessage = cTextMenuHelp testConfig
		, Handlers.Bot.logger = lHandleS
		, Handlers.Bot.base = bsHandleS
		}
	  let testMessage = Message {mData = Msg (T.pack textMessage), mID = 1, mUser = 2}
          execState (Handlers.Bot.makeReaction bHandleS testMessage) "" `shouldBe` (mconcat $ replicate (cRepeatCount testConfig) (T.pack textMessage))
	  let testMessage = Message {mData = Gif (T.pack textMessage), mID = 2, mUser = 3}
          execState (Handlers.Bot.makeReaction bHandleS testMessage) "" `shouldBe` (mconcat $ replicate (cRepeatCount testConfig) (T.pack textMessage))
      it "User can change repeat count" $ do
        property $ \repeatMessage -> do
  	  let lHandleS = Handlers.Logger.Handle
	       { Handlers.Logger.levelLogger = Debug
	       , Handlers.Logger.writeLog = \_ -> pure ()
	       } :: Handlers.Logger.Handle (State T.Text)
          let bsHandleS = Handlers.Base.Handle
	        {  Handlers.Base.findUser = \user -> pure $ Just (5 :: RepeatCount)
	        ,  Handlers.Base.updateUser = \user repeatCount -> pure ()
	        }
          let bHandleS = Handlers.Bot.Handle
		{ Handlers.Bot.sendMessage = \(Message {mData = msg}) -> case msg of
									    Msg text -> put text >> pure ()
									    _ -> pure ()
		, Handlers.Bot.repeatMessage = T.pack repeatMessage
		, Handlers.Bot.getMessage = pure (Message {mData = Msg "1", mID = 1, mUser = 2})
		, Handlers.Bot.logger = lHandleS
		, Handlers.Bot.base = bsHandleS
		}
	  let testMessage = Message {mData = Command "/repeat", mID = 1, mUser = 2}
          execState (Handlers.Bot.makeReaction bHandleS testMessage) "no repeat menu" `shouldBe` (T.pack repeatMessage <> "5")
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
-- makeReaction :: (Monad m) => Handle m -> Message -> m ()
-- makeReaction h msg = do
--   let logHandle = logger h
--   HL.logMessage logHandle Debug "бот рассматривает поступивщее сообщение"
--   case dataMsg of
--     Msg _ -> do
--       HL.logMessage logHandle Debug "Боту было передано текстовое сообщение"
--       count <- Handlers.Base.giveRepeatCountFromBase (base h) user 
--       mapM_ (sendMessage h) (replicate count msg)
--     Gif _ -> do
--       HL.logMessage logHandle Debug "Боту было передано gif сообщение"
--       count <- Handlers.Base.giveRepeatCountFromBase (base h) user 
--       mapM_ (sendMessage h) (replicate count msg)
--     Command t -> do
--       HL.logMessage logHandle Debug "Боту было передана команда"
--       case t of
--         "/help" -> sendMessage h (msg {mData = Msg $ helpMessage h})
--         "/repeat" -> changeRepeatCountForUser h user
--         _ -> error "unknow command"
--     Query i -> do 
--       HL.logMessage logHandle Debug "Боту было передан ответ на запрос о количестве повторений"
--       Handlers.Base.updateUser (base h) user i
--     KeyboardMenu -> pure ()
--     otherwise -> do
--       HL.logMessage logHandle Error "Пришло неизвестное сообщение"
--       error "unknow mData Message"
--     where dataMsg = mData msg
--           id = mID msg
--           user = mUser msg
-- changeRepeatCountForUser :: (Monad m) => Handle m -> User -> m ()
-- changeRepeatCountForUser h user = do
--   let logHandle = logger h
--   HL.logMessage logHandle Debug "запрашиваем количество повторений в базе для пользователя"
--   count <- Handlers.Base.giveRepeatCountFromBase (base h) user
--   let msg = Message {mUser = user} --когда команда /repeat, почему-то стало вылетать здесь
--   sendMessage h (msg {mData = Msg $ (repeatMessage h) <> T.pack (show count) }) 
--   sendMessage h (msg {mData = KeyboardMenu}) 
--   answer <- getMessage h
--   if not (isCorrectRepeatCount answer)
--   then do
--     HL.logMessage logHandle Warning "Пользователь вводит некорректное значение количества повторов"
--     changeRepeatCountForUser h user
--   else do
--     case mData answer of
--       Msg t -> do
--         let query' = read $ T.unpack t :: DataFromButton
-- 	makeReaction h (msg {mData = Query query', mUser = mUser answer})
--       Query i -> makeReaction h (msg {mData = Query i, mUser = mUser answer})
--       otherwise -> do
--         HL.logMessage logHandle Error "Пришло неизвестное сообщение"
--         error "answer uncorrect"
  describe "Client logic" $ do
    it "No logic, no test" $ do
      Debug `shouldBe` Debug
  
  describe "Dispatcher logic" $ do
    it "No logic, no test" $ do
      Debug `shouldBe` Debug

  describe "Logger logic" $ do
    context "Logger write log message if its lvl >= lvl log from Config" $ do
      let lHandleS' = logHandleS testConfig
      let lHandleS = lHandleS' {Handlers.Logger.levelLogger = Debug}
      it "Debug    vs  Debug" $ do
        execState (Handlers.Logger.logMessage lHandleS Debug "New log") "Old log" `shouldBe` "[Debug] New log"
	
      it "Waring   vs  Debug" $ do
        execState (Handlers.Logger.logMessage lHandleS Warning "New log") "Old log" `shouldBe` "[Warning] New log"
      it "Error    vs  Debug" $ do
        execState (Handlers.Logger.logMessage lHandleS Error "New log") "Old log" `shouldBe` "[Error] New log"
      it "Fatal    vs  Debug" $ do
        execState (Handlers.Logger.logMessage lHandleS Fatal "New log") "Old log" `shouldBe` "[Fatal] New log"
      let lHandleS = lHandleS' {Handlers.Logger.levelLogger = Warning}
      it "Warning  vs  Warning" $ do
        execState (Handlers.Logger.logMessage lHandleS Warning "New log") "Old log" `shouldBe` "[Warning] New log"
      it "Error    vs  Warning" $ do
        execState (Handlers.Logger.logMessage lHandleS Error "New log") "Old log" `shouldBe` "[Error] New log"
      it "Fatal    vs  Warning" $ do
        execState (Handlers.Logger.logMessage lHandleS Fatal "New log") "Old log" `shouldBe` "[Fatal] New log"
      let lHandleS = lHandleS' {Handlers.Logger.levelLogger = Error}
      it "Error    vs  Error" $ do
        execState (Handlers.Logger.logMessage lHandleS Error "New log") "Old log" `shouldBe` "[Error] New log"
      it "Fatal    vs  Error" $ do
        execState (Handlers.Logger.logMessage lHandleS Fatal "New log") "Old log" `shouldBe` "[Fatal] New log"
      let lHandleS = lHandleS' {Handlers.Logger.levelLogger = Fatal}
      it "Fatal    vs  Fatal" $ do
        execState (Handlers.Logger.logMessage lHandleS Fatal "New log") "Old log" `shouldBe` "[Fatal] New log"

    context "Logger don't write log message if its lvl < lvl log from Config" $ do
      let lHandleS' = logHandleS testConfig
      let lHandleS = lHandleS' {Handlers.Logger.levelLogger = Warning}
      it "Debug    vs  Warning" $ do
        execState (Handlers.Logger.logMessage lHandleS Debug "New log") "Old log" `shouldNotBe` "[Debug] New log"
      let lHandleS = lHandleS' {Handlers.Logger.levelLogger = Error}
      it "Debug    vs  Error" $ do
        execState (Handlers.Logger.logMessage lHandleS Debug "New log") "Old log" `shouldNotBe` "[Debug] New log"
      it "Warning  vs  Error" $ do
        execState (Handlers.Logger.logMessage lHandleS Warning "New log") "Old log" `shouldNotBe` "[Warning] New log"
      let lHandleS = lHandleS' {Handlers.Logger.levelLogger = Fatal}
      it "Debug    vs  Fatal" $ do
        execState (Handlers.Logger.logMessage lHandleS Debug "New log") "Old log" `shouldNotBe` "[Debug] New log"
      it "Warning  vs  Fatal" $ do
        execState (Handlers.Logger.logMessage lHandleS Warning "New log") "Old log" `shouldNotBe` "[Warning] New log"
      it "Error    vs  Fatal" $ do
        execState (Handlers.Logger.logMessage lHandleS Error "New log") "Old log" `shouldNotBe` "[Error] New log"
-- logMessage :: (Monad m) => Handle m -> Log -> T.Text -> m ()
-- logMessage h lvl msg 
--   | lvl >= (levelLogger h) = writeLog h (mconcat["[",T.pack $ show lvl,"] ", msg])
--   | otherwise = pure ()

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

