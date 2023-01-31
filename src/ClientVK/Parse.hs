module ClientVK.Parse (justKeyBoard) where

import ClientVK.TData (Keyboard(..), Button(..), TParse(..), TParseQuery(..))
import Data.Aeson 
import Data.Text as T (unpack)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L (toStrict)

instance FromJSON TParseQuery where
  parseJSON (Object v) = do
    updateId   <- v .: "result"
                    >>= \r -> Prelude.head r .: "update_id"
    chatId     <- v .: "result" 
                    >>= \r -> Prelude.head r .: "callback_query" 
                    >>= (.: "message")  
                    >>= (.: "chat")  
                    >>= (.: "id")  
    dataButton <- v .: "result"
                    >>= \r -> Prelude.head r .: "callback_query"
                    >>= (.: "data")
    queryId <- v .: "result"
                    >>= \r -> Prelude.head r .: "callback_query"
                    >>= (.: "id")
    return  TParseQuery {
                           qUpdateID = updateId
                         , qChatID   = chatId 
	                 , qDataFromButton = read $ T.unpack dataButton
			 , qQueryID = queryId
		         }
instance FromJSON TParse where
  parseJSON (Object v) = do
    updateId <- v .: "result" 
                  >>= \m -> Prelude.head m .: "update_id" 
    chatId   <- v .: "result" 
                  >>= \m -> Prelude.head m .: "message" 
                  >>= (.: "chat")  
                  >>= (.: "id")  
    message  <- v .: "result"
                  >>= \m -> Prelude.head m .: "message" 
                  >>= \t -> t .:? "text"
                  >>= \case 
                        Just message -> return $ Left message
                        Nothing -> t .: "animation"
                          >>= (.: "file_id") 
                          >>= \gif -> return $ Right gif
 
    return   TParse { 
                      tUpdateID = updateId
                    , tChatID   = chatId 
                    , tMessage  = message 
		    , keyboardMenu = Nothing -- default Nothing
		    , isCommand = False -- default
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
