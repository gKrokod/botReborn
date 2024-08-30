{-# LANGUAGE TypeApplications #-}

module ClientTM.Parse (Keyboard, justKeyBoard, UnknownMessage (..), BoxMessage (..)) where

import Data.Aeson (FromJSON, ToJSON, Value (..), encode, parseJSON, (.:), (.:?))
import Data.Aeson.Types (parseFail, prependFailure, typeMismatch)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L (toStrict)
import Data.Text as T (Text, pack, unpack)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Types (Data (..), DataFromButton (..), ID (..), Message (..), User (..))

newtype Keyboard = Keyboard
  { inline_keyboard :: [[Button]]
  }
  deriving (Show, Generic)

data Button = Button
  { text :: T.Text,
    callback_data :: T.Text
  }
  deriving (Show, Generic)

newtype UnknownMessage = UnknownMessage {uID :: ID} -- another message from telegram client
  deriving (Show)

newtype BoxMessage = BoxMessage {unboxMessage :: Message} -- because had orphan instance
  deriving (Show)

justKeyBoard :: Maybe BC.ByteString
justKeyBoard = Just $ L.toStrict $ encode menuForRepeatCount

menuForRepeatCount :: Keyboard
menuForRepeatCount =
  Keyboard
    { inline_keyboard = [map ((\t -> Button {text = t, callback_data = t}) . T.pack . show @Integer) [1 .. 5]]
    }

instance ToJSON Keyboard

instance ToJSON Button

instance FromJSON UnknownMessage where
  parseJSON (Object v) = do
    updateId <-
      v .: "result"
        >>= \case
          [] -> parseFail "haven't unknown message"
          (h : _) -> h .: "update_id"
    return UnknownMessage {uID = ID updateId}
  parseJSON invalid = prependFailure "parsing Unknown Message failed, " (typeMismatch "Object" invalid)

instance FromJSON BoxMessage where
  parseJSON (Object v) = do
    updateId <-
      v .: "result"
        >>= \case
          [] -> parseFail "haven't message"
          (h : _) -> h .: "update_id"
    chatId <-
      v .: "result"
        >>= \case
          [] -> parseFail "haven't message"
          (h : _) ->
            h .:? "message"
              >>= \case
                Just t ->
                  t .: "chat"
                    >>= (.: "id")
                Nothing ->
                  h .: "callback_query"
                    >>= (.: "message")
                    >>= (.: "chat")
                    >>= (.: "id")
    message <-
      v .: "result"
        >>= \case
          [] -> parseFail "haven't message"
          (h : _) ->
            h .:? "message"
              >>= \case
                Just t ->
                  t .:? "text"
                    >>= \case
                      Just message -> pure $ Msg message
                      Nothing ->
                        t .: "animation"
                          >>= (.: "file_id")
                          >>= pure . Gif
                Nothing ->
                  h .: "callback_query"
                    >>= (.: "data")
                    >>= \x -> case readMaybe . T.unpack $ x of
                      Nothing -> parseFail "bad data from Button"
                      Just n -> pure $ Query $ DataFromButton n
    return
      BoxMessage {unboxMessage = Message {mID = ID updateId, mUser = User chatId, mData = message}}
  parseJSON invalid = prependFailure "parsing Message failed, " (typeMismatch "Object" invalid)
