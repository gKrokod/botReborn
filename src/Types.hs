module Types where
import GHC.Generics
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC -- добавить потом через скобки импорт 

type User = Int
type RepeatCount = Int
type ID = Int
type DataFromButton = Int
type LastMessage = Message

data Data t i = Msg t | Gif t | Command t | KeyboardMenu | Query i deriving (Show, Eq)

data Message = Message 
  {
    mData :: Data T.Text DataFromButton 
  , mID :: ID
  , mUser :: User
  } deriving (Eq, Show) --show for test dispatcher

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
 ,  cLvlLog :: Log
 } deriving Show

data Mode = ConsoleBot | TelegramBot deriving (Show, Eq)

data Log = Debug | Warning | Error | Fatal deriving (Eq, Ord, Show)

    -- Debug — запись масштабных переходов состояний, например, обращение к базе данных, старт/пауза сервиса, успешная обработка записи и пр.
    --
    -- Warning — нештатная ситуация, потенциальная проблема, может быть странный формат запроса или некорректный параметр вызова.
    --
    -- Error — типичная ошибка.
    --
    -- Fatal — тотальный сбой работоспособности, когда нет доступа к базе данных или сети, сервису не хватает места на жестком диске.
--
-- data TParse = TParse
--  {
--     tUpdateID :: UpdateID
--  ,  tChatID :: ChatID
--  ,  tMessage :: Either Message Gif
--  ,  keyboardMenu :: Maybe BC.ByteString
--  ,  isCommand :: Bool
--  } deriving Show
--
-- data TParseQuery = TParseQuery 
--  {
--    qUpdateID :: UpdateID
--  , qChatID :: ChatID
--  , qQueryID :: QueryID
--  , qDataFromButton :: DataFromButton
--  } deriving Show
--
data Keyboard = Keyboard {
                  inline_keyboard :: [[Button]]
                } deriving (Show, Generic)

data Button = Button {
                text :: T.Text
	      , callback_data :: T.Text	
              } deriving (Show, Generic)

