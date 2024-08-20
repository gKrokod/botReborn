{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Types (Message (..), Log (..), LastMessage, User, RepeatCount, Data (..), DataFromButton, ID, defaultMessage) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (..), ToJSON (..))

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
  deriving (Eq, Show) 


data Log = Debug | Warning | Error | Fatal
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultMessage :: Message
defaultMessage = Message {mID = -1, mUser = -1, mData = Msg "fake message for -Wall and -Werror"}
