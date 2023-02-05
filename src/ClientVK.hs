module ClientVK (fetch, carryAway) where

-- тут реализация vk версии
import Types 
import qualified Data.Text as T (Text) 
import ClientVK.HttpMessage ( buildGetRequest
                            , buildTextSendRequest
			    , buildGifSendRequest
			    , buildKeyboardSendRequest)
import Network.HTTP.Simple ( httpLBS
                           , getResponseBody
			   , getResponseStatusCode)
import qualified Data.ByteString.Char8 as BC (pack)
import Data.Aeson (decode)

fetch :: Config -> Maybe LastMessage -> IO (Maybe Message)
fetch c lm = do
  let cfg = c {cOffset = maybe "-1" (BC.pack . show . succ . mID) lm } 
  response <- httpLBS $ buildGetRequest cfg
  let status = getResponseStatusCode response
  if (404 == status || status == 301) then print "Bot Server 404 or 301" >> pure Nothing
  else do
    let msg = decode $ getResponseBody $ response
    case msg of
      Nothing -> pure Nothing
      Just m -> pure $ Just $ makeMessage (mData m) m
  
makeMessage :: Data T.Text DataFromButton -> Message -> Message
makeMessage (Msg t) msg = case t of
  "/help"   -> msg {mData = Command "/help"}
  "/repeat" -> msg {mData = Command "/repeat"}
  otherwise -> msg 
makeMessage _ msg = msg

carryAway :: Config -> Message -> IO ()
carryAway cfg msg = case mData msg of
                      Msg t -> httpLBS (buildTextSendRequest cfg msg) >> pure ()
		      Gif t -> httpLBS (buildGifSendRequest cfg msg) >> pure ()
		      KeyboardMenu -> httpLBS (buildKeyboardSendRequest cfg msg) >> pure ()
		      otherwise -> print "carryAway wrong message" >> pure ()

