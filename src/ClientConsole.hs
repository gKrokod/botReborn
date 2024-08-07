module ClientConsole (fetch, carryAway) where

-- implementation console verion

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.System as Time
import Types (Data (..), LastMessage, Message (..), defaultMessage)

fetch :: Maybe LastMessage -> IO (Maybe Message)
fetch lm = do
  m <- getLine
  time <- Time.getSystemTime
  let msg = defaultMessage {mID = fromIntegral $ Time.systemSeconds time}
  -- let msg = Message {mID = fromIntegral $ Time.systemSeconds time, mUser = 1, mData = Msg "fake"}
  case lm of
    Nothing -> pure $ Just $ makeMessage m msg
    Just m' ->
      if mID m' == mID msg
        then pure Nothing
        else pure $ Just $ makeMessage m msg

makeMessage :: String -> Message -> Message
makeMessage t msg = case t of
  "/help" -> msg {mData = Command "/help"}
  "/start" -> msg {mData = Command "/help"}
  "/repeat" -> msg {mData = Command "/repeat"}
  _ -> msg {mData = Msg $ T.pack t}

carryAway :: Message -> IO ()
carryAway msg = case mData msg of
  -- Query i      -> print i
  Msg t -> TIO.putStrLn t
  KeyboardMenu -> TIO.putStrLn "Type a new repeat count [1..5]: "
  _ -> pure ()
