import Test.Hspec
import Test.QuickCheck
import Types
import Control.Monad.Identity
import qualified Handlers.Logger
import qualified Handlers.Base
import qualified Data.Text as T
import qualified Data.Text.IO as TIO



main :: IO ()
main = hspec $ do
  describe "Base logic" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Bot logic" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Client logic" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Dispatcher logic" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

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
		 ,  Handlers.Base.saveMessage = Base.saveMessage stackMessage
		 ,  Handlers.Base.eraseMessage = Base.eraseMessage stackMessage
		 ,  Handlers.Base.findUser = Base.findUser base
		 ,  Handlers.Base.updateUser = Base.updateUser base
		 ,  Handlers.Base.logger = logHandle
		 }
-- data Handle m = Handle 
--   {  defaultRepeatCount :: RepeatCount
--   ,  readStackMessage :: m (Maybe Message, Maybe LastMessage)
--   ,  saveMessage :: Message -> m ()
--   ,  eraseMessage :: Message -> m ()
--   ,  findUser :: User -> m (Maybe RepeatCount)
--   ,  updateUser :: User -> RepeatCount -> m ()
--   ,  logger :: HL.Handle m  
--   }
