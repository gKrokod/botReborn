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

  describe "Bot logic" $ modifyMaxSuccess (const 1000) $ do
    context "User can change repeat counts only in range [1..5]" $ do
      it "Input: Int" $ do
	property $ \answer -> do
	  let msg = Message {mData = Query answer, mID = 1, mUser = 2}
	  Handlers.Bot.isCorrectRepeatCount msg == (1 <= answer && answer <= 5)

      it "Input: Text" $ do
	property $ \answer -> do
	  let numberMessage = maybe 0 id (readMaybe answer :: Maybe Int)
	  let msg = Message {mData = Msg (T.pack answer), mID = 1, mUser = 2}	 
	  Handlers.Bot.isCorrectRepeatCount msg == (1 <= numberMessage && (numberMessage <= 5))
    
    context "Correct answer on:" $ do
      it "/help" $ do
        Debug `shouldBe` Debug

  describe "Client logic" $ do
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

