module Clients.Console.Internal.Fetch (fetch) where

import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.System as Time
import Types (Data (..), ID (..), LastMessage, Message (..), Messages (..), User (..))

fetch :: Maybe LastMessage -> IO (Maybe Message)
fetch lm = do
  m <- TIO.getLine
  time <- Time.getSystemTime
  let msg = Message {mID = ID (fromIntegral $ Time.systemSeconds time), mUser = User (-1), mData = NoMsg}
  case lm of
    Nothing -> pure $ Just $ makeMessage m msg
    Just m' ->
      if mID m' == mID msg
        then pure Nothing
        else pure $ Just $ makeMessage m msg
