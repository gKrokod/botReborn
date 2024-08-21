{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Types (Message (..), Log (..), LastMessage, User (..), RepeatCount (..), Data (..), DataFromButton (..), ID (..), defaultMessage) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype User = User {giveUser :: Int}
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

newtype RepeatCount = RepeatCount Int
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

newtype ID = ID {giveId :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DataFromButton = DataFromButton {dataFromButton :: Int}
  deriving stock (Show, Eq, Generic, Read)

type LastMessage = Message

data Data t i = Msg t | Gif t | Command t | KeyboardMenu | Query i deriving (Show, Eq)

data Message = Message
  { mData :: Data T.Text DataFromButton,
    mID :: ID,
    mUser :: User
  }
  deriving (Eq, Show)

data Log = Debug | Warning | Error | Fatal
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultMessage :: Message
defaultMessage = Message {mID = ID (-1), mUser = User (-1), mData = Msg "fake message for -Wall and -Werror"}
