module Clients.Telegram.Internal.Fetch (fetch) where

import Clients.Telegram.Internal.HttpMessage
  ( buildGetRequest,
  )
import Clients.Telegram.Internal.Parse (BoxMessage (..), UnknownMessage (..))
import Config (Config (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Aeson (eitherDecode)
import qualified Data.Text as T (pack)
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseStatusCode,
    httpLBS,
  )
import Types (Data (..), ID (..), LastMessage, Message (..), Messages (..), User (..))

fetch :: Config -> Maybe LastMessage -> IO (Maybe Message)
fetch cfg lm = do
  response' <- try $ httpLBS $ buildGetRequest (cfg {cOffset = maybe "-1" (T.pack . show . succ . giveId . mID) lm})
  case response' of
    Left e -> do
      print (e :: SomeException)
      threadDelay 1000000
      fetch cfg lm
    Right response -> do
      let status = getResponseStatusCode response
      when (404 == status || status == 301) (TIO.putStrLn "Error! Bot Server 404 or 301")
      let msg = eitherDecode $ getResponseBody response -- messages : text, gif
      let umsg = eitherDecode $ getResponseBody response -- another messages
      case (msg, umsg) of
        (Right m, _) -> pure $ Just $ makeMessage (mData (unboxMessage m)) (unboxMessage m)
        (_, Right m) -> fetch cfg (Just $ Message {mID = uID m, mData = NoMsg, mUser = User (-1)})
        _ -> do
          threadDelay 1000000
          pure Nothing
