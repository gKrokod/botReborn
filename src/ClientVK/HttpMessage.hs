module ClientVK.HttpMessage where
import qualified Data.Text.Encoding as E (encodeUtf8)
import Network.HTTP.Simple (defaultRequest, setRequestPort, setRequestSecure, setRequestMethod,
 setRequestHost, parseRequest, Request, setRequestPath, setRequestQueryString)
import qualified Data.ByteString.Char8 as BC (pack)
import Data.Function ((&))
import ClientVK.Parse (justKeyBoard)
import Types (Config(..), Message(..), Data(..))

buildGetRequest :: Config -> Request
buildGetRequest cfg =
    setRequestHost (cfg & cBotHost)
  $ setRequestMethod (cfg & cMethod) 
  $ setRequestSecure (cfg & cSecure)
  $ setRequestQueryString ([("offset", Just $ cfg & cOffset), ("timeout", Just $ cfg & cTimeOut)])
  $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, "/getUpdates"])
  $ setRequestPort (cfg & cPort)
  $ defaultRequest

buildTextSendRequest :: Config -> Message -> Request
buildTextSendRequest  cfg msg = do
    setRequestQueryString ([("chat_id", msg & Just . BC.pack . show . mUser), ("text", Just $ E.encodeUtf8 $ textMessage), ("reply_markup", Nothing)])
  $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, "/sendMessage"])
  $ buildDefaultSendRequest cfg
    where (Msg textMessage) = mData msg

buildGifSendRequest :: Config -> Message -> Request
buildGifSendRequest  cfg msg = do
    setRequestQueryString ([("chat_id", msg & Just . BC.pack . show . mUser), ("animation", Just $ E.encodeUtf8 $ gifMessage), ("reply_markup", Nothing)])
  $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, "/sendAnimation"])
  $ buildDefaultSendRequest cfg
    where (Gif gifMessage) = mData msg

buildKeyboardSendRequest :: Config -> Message -> Request
buildKeyboardSendRequest cfg msg = do
    setRequestQueryString ([("chat_id", msg & Just . BC.pack . show . mUser), ("text", Just $ E.encodeUtf8 $ "Для выбора нового значения введите цифру, либо нажмите кнопку клавиатуры"), ("reply_markup", justKeyBoard)])
  $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, "/sendMessage"])
  $ buildDefaultSendRequest cfg

buildDefaultSendRequest :: Config -> Request
buildDefaultSendRequest cfg =
    setRequestHost (cfg & cBotHost)
  $ setRequestMethod (cfg & cMethod)
  $ setRequestSecure (cfg & cSecure)
  $ setRequestPort (cfg & cPort)
  $ defaultRequest
