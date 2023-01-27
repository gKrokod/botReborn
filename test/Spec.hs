import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Read
import Types
import Control.Monad.Identity
import Data.Char (isDigit)
import qualified Handlers.Logger
import qualified Handlers.Base
import qualified Handlers.Bot
import qualified Data.Text as T
import qualified Data.Text.IO as TIO



main :: IO ()
main = hspec $ do
  describe "Base logic" $ do
    let bHandle = baseHandle testConfig
    it "returns the default repeat count from Config for new users" $
      property $ \user -> runIdentity (Handlers.Base.giveRepeatCountFromBase bHandle user) == cRepeatCount testConfig

    it "returns default repeat counts from Config for the new user" $
      property $ \count -> do
        let bHandle = baseHandle (testConfig {cRepeatCount = count})
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
  --
  -- describe "Dispatcher logic" $ do
  --   it "returns the first element of a list" $ do
  --     head [23 ..] `shouldBe` (23 :: Int)

  describe "Logger logic" $ do
    it "No logic, no test" $ do
      Debug `shouldBe` Debug

testConfig :: Config 
testConfig = Config
	     {  cRepeatCount = 3 :: RepeatCount
	     ,  cTextMenuHelp = "Help menu:" :: T.Text 
	     ,  cTextMenuRepeat = "Repeat menu:" :: T.Text
	     ,  cLvlLog = Debug :: Log
	     } 

logHandle :: Config -> Handlers.Logger.Handle Identity
logHandle cfg = Handlers.Logger.Handle
	       { Handlers.Logger.levelLogger = cLvlLog cfg-- Fatal
	       , Handlers.Logger.writeLog = \text -> pure () 
	       }

baseHandle :: Config -> Handlers.Base.Handle Identity
baseHandle cfg = Handlers.Base.Handle
		 {  Handlers.Base.defaultRepeatCount = cRepeatCount cfg
		 ,  Handlers.Base.readStackMessage = pure (Nothing, Nothing)
		 ,  Handlers.Base.saveMessage = \_ -> pure ()
		 ,  Handlers.Base.eraseMessage = \_ -> pure ()
		 ,  Handlers.Base.findUser = \user -> pure (Just $ cRepeatCount cfg)
		 ,  Handlers.Base.updateUser = \user count -> pure ()
		 ,  Handlers.Base.logger = logHandle cfg
		 }

botHandle :: Config -> Handlers.Bot.Handle Identity
botHandle cfg = Handlers.Bot.Handle 
		{ Handlers.Bot.getMessage = pure (Message {mData = Msg ("Test message"), mID = 1, mUser = 2})
		, Handlers.Bot.sendMessage = \text -> pure ()
		, Handlers.Bot.base = baseHandle cfg
		, Handlers.Bot.helpMessage = cTextMenuHelp cfg
		, Handlers.Bot.repeatMessage = cTextMenuRepeat cfg
		, Handlers.Bot.logger = logHandle cfg
		}

-- data Handle m = Handle
--   {  getMessage :: m (Message)
--   ,  sendMessage :: Message -> m ()
--   ,  base :: Handlers.Base.Handle m
--   ,  helpMessage :: T.Text
--   ,  repeatMessage :: T.Text
--   ,  logger :: HL.Handle m  
--   }
