module ClientTM (fetch, carryAway) where

-- implementation vk version

import ClientTM.HttpMessage
  ( buildGetRequest,
    buildGifSendRequest,
    buildKeyboardSendRequest,
    buildTextSendRequest,
  )
import ClientTM.Parse (BoxMessage (..), UnknownMessage (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseStatusCode,
    httpLBS,
  )
import Types (Config (..), Data (..), DataFromButton, LastMessage, Message (..), defaultMessage)

fetch :: Config -> Maybe LastMessage -> IO (Maybe Message)
fetch cfg lm = do
  response' <- try $ httpLBS $ buildGetRequest (cfg {cOffset = maybe "-1" (BC.pack . show . succ . mID) lm})
  case response' of
    Left e -> do
      print (e :: SomeException)
      threadDelay 10
      fetch cfg lm
    Right response -> do
      let status = getResponseStatusCode response
      when (404 == status || status == 301) (TIO.putStrLn "Error! Bot Server 404 or 301")
      let msg = decode $ getResponseBody response -- messages : text, gif
      let umsg = decode $ getResponseBody response -- another messages
      case (msg, umsg) of
        (Just m, _) -> pure $ Just $ makeMessage (mData (unboxMessage m)) (unboxMessage m)
        (_, Just m) -> fetch cfg (Just defaultMessage {mID = uID m})
        _ -> pure Nothing

makeMessage :: Data T.Text DataFromButton -> Message -> Message
makeMessage (Msg t) msg = case t of
  "/help" -> msg {mData = Command "/help"}
  "/start" -> msg {mData = Command "/help"}
  "/repeat" -> msg {mData = Command "/repeat"}
  _ -> msg
makeMessage _ msg = msg

carryAway :: Config -> Message -> IO ()
carryAway cfg msg = case mData msg of
  Msg _ -> void $ httpLBS (buildTextSendRequest cfg msg)
  Gif _ -> void $ httpLBS (buildGifSendRequest cfg msg)
  KeyboardMenu -> void $ httpLBS (buildKeyboardSendRequest cfg msg)
  _ -> void $ TIO.putStrLn "carryAway wrong message"
