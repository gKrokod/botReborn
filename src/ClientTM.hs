module ClientTM (fetch, carryAway) where

-- тут реализация vk версии

import ClientTM.HttpMessage
  ( buildGetRequest,
    buildGifSendRequest,
    buildKeyboardSendRequest,
    buildTextSendRequest,
  )
import ClientTM.Parse (UnknownMessage (..))
import Control.Monad (when)
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.Text as T (Text)
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseStatusCode,
    httpLBS,
  )
import Types (Config (..), Data (..), DataFromButton, LastMessage, Message (..))

fetch :: Config -> Maybe LastMessage -> IO (Maybe Message)
fetch cfg lm = do
  response <- httpLBS $ buildGetRequest (cfg {cOffset = maybe "-1" (BC.pack . show . succ . mID) lm})
  let status = getResponseStatusCode response
  when (404 == status || status == 301) (print "Error! Bot Server 404 or 301")
  let msg = decode $ getResponseBody $ response -- messages : text, gif
  let umsg = decode $ getResponseBody $ response -- another messages
  case (msg, umsg) of
    (Just m, _) -> pure $ Just $ makeMessage (mData m) m
    (_, Just m) -> fetch cfg (Just Message {mID = uID m})
    _ -> pure Nothing

makeMessage :: Data T.Text DataFromButton -> Message -> Message
makeMessage (Msg t) msg = case t of
  "/help" -> msg {mData = Command "/help"}
  "/start" -> msg {mData = Command "/help"}
  "/repeat" -> msg {mData = Command "/repeat"}
  otherwise -> msg
makeMessage _ msg = msg

carryAway :: Config -> Message -> IO ()
carryAway cfg msg = case mData msg of
  Msg t -> httpLBS (buildTextSendRequest cfg msg) >> pure ()
  Gif t -> httpLBS (buildGifSendRequest cfg msg) >> pure ()
  KeyboardMenu -> httpLBS (buildKeyboardSendRequest cfg msg) >> pure ()
  otherwise -> print "carryAway wrong message" >> pure ()
