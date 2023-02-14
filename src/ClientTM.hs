module ClientTM (fetch, carryAway) where

-- тут реализация vk версии

import ClientTM.HttpMessage
  ( buildGetRequest,
    buildGifSendRequest,
    buildKeyboardSendRequest,
    buildTextSendRequest,
  )
import ClientTM.Parse (UnknownMessage (..), WrapMessage (..))
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
import qualified Data.Text.IO as TIO

fetch :: Config -> Maybe LastMessage -> IO (Maybe Message)
fetch cfg lm = do
  response <- httpLBS $ buildGetRequest (cfg {cOffset = maybe "-1" (BC.pack . show . succ . mID) lm})
  let status = getResponseStatusCode response
  when (404 == status || status == 301) (TIO.putStrLn "Error! Bot Server 404 or 301")
  let msg = decode $ getResponseBody $ response -- messages : text, gif
  let umsg = decode $ getResponseBody $ response -- another messages
  case (msg, umsg) of
    (Just m, _) -> pure $ Just $ makeMessage (mData (wMsg m)) (wMsg m)
    (_, Just m) -> fetch cfg (Just Message {mID = uID m, mUser = -1, mData = Msg "fake"})
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
  Msg _ -> httpLBS (buildTextSendRequest cfg msg) >> pure ()
  Gif _ -> httpLBS (buildGifSendRequest cfg msg) >> pure ()
  KeyboardMenu -> httpLBS (buildKeyboardSendRequest cfg msg) >> pure ()
  _ -> TIO.putStrLn "carryAway wrong message" >> pure ()
