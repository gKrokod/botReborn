module Types where
import qualified Data.Text as T

type User = Int
type RepeatCount = Int
type ID = Int
type DataFromButton = Int
-- type Git = T.Text
-- type Msg = T.Text
-- type Command = T.Text
-- type Query = RepeatCount
-- type Keyboard = T.Text


data Data t i = Msg t | Gif t | Command t | Keyboard t | Query i deriving Show

data Message = Message 
  {
    mData :: Data T.Text DataFromButton 
  , mID :: ID
  , mUser :: User
  }


commandHelp = "/help" :: T.Text
commandRepeat = "/repeat" :: T.Text
keyboardMenu = "Vvedite 12345" :: T.Text
