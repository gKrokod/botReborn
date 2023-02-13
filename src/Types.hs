module Types where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

type User = Int

type RepeatCount = Int

type ID = Int

type DataFromButton = Int

type LastMessage = Message

data Data t i = Msg t | Gif t | Command t | KeyboardMenu | Query i deriving (Show, Eq)

data Message = Message
  { mData :: Data T.Text DataFromButton,
    mID :: ID,
    mUser :: User
  }
  deriving (Eq, Show) -- show for test dispatcher

data Config = Config
  { cRepeatCount :: RepeatCount,
    cTextMenuHelp :: T.Text,
    cTextMenuRepeat :: T.Text,
    cApiPath :: BC.ByteString,
    cBotHost :: BC.ByteString,
    cTimeOut :: BC.ByteString,
    cOffset :: BC.ByteString,
    cToken :: BC.ByteString,
    cPort :: Int,
    cMethod :: BC.ByteString,
    cSecure :: Bool,
    cMode :: Mode,
    cLvlLog :: Log
  }
  deriving (Show)

data Mode = ConsoleBot | TelegramBot deriving (Show, Eq)

data Log = Debug | Warning | Error | Fatal deriving (Eq, Ord, Show)

-- Debug — запись масштабных переходов состояний, например, обращение к базе данных, старт/пауза сервиса, успешная обработка записи и пр.
--
-- Warning — нештатная ситуация, потенциальная проблема, может быть странный формат запроса или некорректный параметр вызова.
--
-- Error — типичная ошибка.
--
-- Fatal — тотальный сбой работоспособности, когда нет доступа к базе данных или сети, сервису не хватает места на жестком диске.
