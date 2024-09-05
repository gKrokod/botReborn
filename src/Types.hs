{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

module Types (Message (..), Log (..), LastMessage, User (..), RepeatCount (..), Data (..), DataFromButton (..), ID (..), Messages (..), Command(..)) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype User = User {userId :: Int}
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

newtype RepeatCount = RepeatCount Int
  deriving stock (Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

instance Show RepeatCount where
  show (RepeatCount n) = show n

newtype ID = ID {giveId :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DataFromButton = DataFromButton {dataFromButton :: Int}
  deriving stock (Show, Eq, Generic, Read)

type LastMessage = Message

data Data t i = NoMsg | Msg t | Gif t | KeyboardMenu | Query i | Service Command deriving (Show, Eq)

data Command = Help | Repeat deriving (Show, Eq)

data Message = Message
  { mData :: Data T.Text DataFromButton,
    mID :: ID,
    mUser :: User
  }
  deriving (Eq, Show)

data Log = Debug | Warning | Error | Fatal
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class Messages a where
  makeMessage :: a -> Message -> Message

instance Messages T.Text where
  makeMessage t msg = case t of
    "/help" -> msg {mData = Service Help}
    "/start" -> msg {mData = Service Help}
    "/repeat" -> msg {mData = Service Repeat}
    _ -> msg {mData = Msg t}

instance Messages (Data T.Text DataFromButton) where
  makeMessage (Msg t) msg = makeMessage t msg
  makeMessage _ msg = msg
