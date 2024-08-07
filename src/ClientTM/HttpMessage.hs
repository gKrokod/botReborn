module ClientTM.HttpMessage (buildTextSendRequest, buildGetRequest, buildGifSendRequest, buildKeyboardSendRequest) where

import ClientTM.Parse (justKeyBoard)
import qualified Data.ByteString.Char8 as BC (pack)
import Data.Function ((&))
import qualified Data.Text.Encoding as E (encodeUtf8)
import Network.HTTP.Simple
  ( Request,
    defaultRequest,
    setRequestHost,
    setRequestMethod,
    setRequestPath,
    setRequestPort,
    setRequestQueryString,
    setRequestSecure,
  )
import Types (Config (..), Data (..), Message (..))

buildGetRequest :: Config -> Request
buildGetRequest cfg =
  setRequestHost (cfg & cBotHost) $
    setRequestMethod (cfg & cMethod) $
      setRequestSecure (cfg & cSecure) $
        setRequestQueryString [("offset", Just $ cfg & cOffset), ("timeout", Just $ cfg & cTimeOut)] $
          setRequestPath (mconcat [cfg & cApiPath, cfg & cToken, "/getUpdates"]) $
            setRequestPort
              (cfg & cPort)
              defaultRequest

buildTextSendRequest :: Config -> Message -> Request
buildTextSendRequest cfg msg =
  do
    setRequestQueryString [("chat_id", msg & Just . BC.pack . show . mUser), ("text", Just $ E.encodeUtf8 textMessage), ("reply_markup", Nothing)]
    $ setRequestPath (mconcat [cfg & cApiPath, cfg & cToken, "/sendMessage"])
    $ buildDefaultSendRequest cfg
  where
    textMessage = case mData msg of
      Msg t -> t
      _ -> ""

buildGifSendRequest :: Config -> Message -> Request
buildGifSendRequest cfg msg =
  do
    setRequestQueryString [("chat_id", msg & Just . BC.pack . show . mUser), ("animation", Just $ E.encodeUtf8 gifMessage), ("reply_markup", Nothing)]
    $ setRequestPath (mconcat [cfg & cApiPath, cfg & cToken, "/sendAnimation"])
    $ buildDefaultSendRequest cfg
  where
    gifMessage = case mData msg of
      Gif t -> t
      _ -> ""

buildKeyboardSendRequest :: Config -> Message -> Request
buildKeyboardSendRequest cfg msg =
  do
    setRequestQueryString [("chat_id", msg & Just . BC.pack . show . mUser), ("text", Just $ E.encodeUtf8 "Enter a new number of repeats"), ("reply_markup", justKeyBoard)]
    $ setRequestPath (mconcat [cfg & cApiPath, cfg & cToken, "/sendMessage"])
    $ buildDefaultSendRequest cfg

buildDefaultSendRequest :: Config -> Request
buildDefaultSendRequest cfg =
  setRequestHost (cfg & cBotHost) $
    setRequestMethod (cfg & cMethod) $
      setRequestSecure (cfg & cSecure) $
        setRequestPort
          (cfg & cPort)
          defaultRequest
