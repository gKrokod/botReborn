module Types where
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC -- добавить потом через скобки импорт 

type User = Int
type RepeatCount = Int
type ID = Int
type DataFromButton = Int

data Data t i = Msg t | Gif t | Command t | KeyboardMenu | Query i deriving Show

data Message = Message 
  {
    mData :: Data T.Text DataFromButton 
  , mID :: ID
  , mUser :: User
  }

data Config = Config
 {
    cRepeatCount :: RepeatCount
 ,  cTextMenuHelp :: T.Text  -- check for Russian words
 ,  cTextMenuRepeat :: T.Text -- check for Russian words
 ,  cApiPath :: BC.ByteString
 ,  cBotHost :: BC.ByteString
 ,  cTimeOut :: BC.ByteString
 ,  cOffset :: BC.ByteString
 ,  cToken :: BC.ByteString
 ,  cPort :: Int
 ,  cMethod :: BC.ByteString
 ,  cSecure :: Bool
 ,  cMode :: Mode
 } deriving Show

data Mode = ConsoleBot | TelegramBot deriving (Show, Eq)
