module ClientVK.HttpMessage where
-- import ClientVK.TData (QueryID, TParse(..))
import qualified Data.Text.Encoding as E (encodeUtf8)
import Network.HTTP.Simple --(parseRequest, Request, httpLBS, getResponseBody, getResponseStatusCode, getResponseHeader)
import qualified Data.ByteString.Char8 as BC (pack)
import Data.Function ((&))
import ClientVK.Parse (justKeyBoard)
import Types

buildGetRequest :: Config -> Request
buildGetRequest cfg =
    setRequestHost (cfg & cBotHost)
  $ setRequestMethod (cfg & cMethod) 
  $ setRequestSecure (cfg & cSecure)
  $ setRequestQueryString ([("offset", Just $ cfg & cOffset), ("timeout", Just $ cfg & cTimeOut)])
  -- $ setRequestQueryString ([("offset", Just (E.encodeUtf8 "240950452")), ("timeout", Just $ cfg & cTimeOut)])
  $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, "/getUpdates"])
  $ setRequestPort (cfg & cPort)
  $ defaultRequest
--
-- buildSendRequest :: Config -> Message -> Request
-- buildSendRequest cfg (Message msg) =
--     setRequestHost (cfg & cBotHost)
--   $ setRequestMethod (cfg & cMethod)
--   $ setRequestSecure (cfg & cSecure)
--   $ setRequestQueryString ([("chat_id", msg & Just . BC.pack . show . mUser ), queryMsg $ msg & tMessage, ("reply_markup", user & keyboardMenu)  ])
--   $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, either (\_ -> "/sendMessage") (\_ -> "/sendAnimation") (user & tMessage) ])
--   $ setRequestPort (cfg & cPort)
--   $ defaultRequest
--     where queryMsg (Left msg1) = ("text", Just $ E.encodeUtf8 msg)
--           queryMsg (Right msg2) = ("animation", Just $ E.encodeUtf8 msg)

-- makeResponse :: Config -> TParse -> TParse
-- makeResponse cfg user = case user & tMessage of
--   Left "/help" -> user {tMessage = Left $ cfg & cTextMenuHelp, isCommand = True}
--   Left "/repeat" -> user {tMessage = Left $ cfg & cTextMenuRepeat, keyboardMenu = justKeyBoard, isCommand = True}
--   Left msg -> user
--   Right msg -> user

-- buildCallBackQuery :: Config -> QueryID -> Request
-- buildCallBackQuery cfg q =
--     setRequestHost (cfg & cBotHost)
--   $ setRequestMethod (cfg & cMethod) 
--   $ setRequestSecure (cfg & cSecure)
--   $ setRequestQueryString ([("callback_query_id", Just $ E.encodeUtf8 $ q), ("text", Just "Your answer is saved"), ("show_alert", Just $ "false")])
--   $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, "/answerCallbackQuery"])
--   $ setRequestPort (cfg & cPort)
--   $ defaultRequest
