module ClientVK.Parse where

import Data.Aeson 
import Data.Text as T (unpack)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L (toStrict)
import Types (Message(..), Keyboard(..), Button(..), Data(..))

instance FromJSON Message where
  parseJSON (Object v) = do
    updateId <- v .: "result" 
                  >>= \m -> Prelude.head m .: "update_id" 
    chatId  <- v .: "result"
                  >>= \m -> Prelude.head m .:? "message" 
                  >>= \case
                        Just t -> t .: "chat"
                                  >>= (.: "id")
                        Nothing -> Prelude.head m .: "callback_query" 
                                   >>= (.: "message")
                                   >>= (.: "chat")
                                   >>= (.: "id")
    message  <- v .: "result"
                  >>= \m -> Prelude.head m .:? "message" 
                  >>= \case
                        Just t -> t .:? "text"
                                  >>= \case 
                                        Just message -> pure $ Msg message
                                        Nothing -> t .: "animation"
                                                   >>= (.: "file_id") 
                                                   >>= pure . Gif
                        Nothing -> Prelude.head m .: "callback_query" 
                                   >>= (.: "data")
                                   >>= pure . Query . read . T.unpack 
 
    return   Message { mID = updateId
                     , mUser   = chatId 
                     , mData = message 
                     }

instance ToJSON Keyboard
instance ToJSON Button

justKeyBoard :: Maybe BC.ByteString
justKeyBoard = Just $ L.toStrict $ encode menuForRepeatCount 

menuForRepeatCount :: Keyboard
menuForRepeatCount = Keyboard { inline_keyboard = [[Button {text = "1", callback_data = "1"}
                              , Button {text = "2", callback_data = "2"}
                              , Button {text = "3", callback_data = "3"}
                              , Button {text = "4", callback_data = "4"}
                              , Button {text = "5", callback_data = "5"}
                               ]]
	                      }
