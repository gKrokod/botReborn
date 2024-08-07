module ClientTM.Parse (Keyboard, justKeyBoard, UnknownMessage (..), BoxMessage (..)) where

import Data.Aeson (FromJSON, ToJSON, Value (..), encode, parseJSON, (.:), (.:?))
import Data.Aeson.Types (parseFail, prependFailure, typeMismatch)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L (toStrict)
import Data.Text as T (Text, unpack)
import GHC.Generics (Generic)
import Types (Data (..), ID, Message (..))

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

newtype BoxMessage = BoxMessage {unboxMessage :: Message} -- because had orphan instance

justKeyBoard :: Maybe BC.ByteString
justKeyBoard = Just $ L.toStrict $ encode menuForRepeatCount

menuForRepeatCount :: Keyboard
menuForRepeatCount =
  Keyboard
    { inline_keyboard =
        [ [ Button {text = "1", callback_data = "1"},
            Button {text = "2", callback_data = "2"},
            Button {text = "3", callback_data = "3"},
            Button {text = "4", callback_data = "4"},
            Button {text = "5", callback_data = "5"}
          ]
        ]
    }

instance ToJSON Keyboard

instance ToJSON Button

instance FromJSON UnknownMessage where
  parseJSON (Object v) = do
    updateId <-
      v .: "result"
        >>= \case
          [] -> parseFail "haven't unknwon message"
          (h : _) -> h .: "update_id"
    return UnknownMessage {uID = updateId}
  parseJSON invalid = prependFailure "parsing UnknownMessage failed, " (typeMismatch "Object" invalid)

-- instance FromJSON Message, rebuild because -Wall, -Werror
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
          [] -> parseFail "haven't message" -- v .: "emptyListMakeNothing" >>= pure . Gif
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
                    >>= pure . Query . read . T.unpack
    return
      BoxMessage {unboxMessage = Message {mID = updateId, mUser = chatId, mData = message}}
  parseJSON invalid = prependFailure "parsing Message failed, " (typeMismatch "Object" invalid)
