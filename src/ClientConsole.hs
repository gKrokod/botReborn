module ClientConsole (fetch, carryAway) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.System as Time
import Types (Data (..), LastMessage, Message (..), defaultMessage, ID (..))

fetch :: Maybe LastMessage -> IO (Maybe Message)
fetch lm = do
  m <- getLine
  time <- Time.getSystemTime
  let msg = defaultMessage {mID = ID (fromIntegral $ Time.systemSeconds time)}
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
  Msg t -> TIO.putStrLn t
  KeyboardMenu -> TIO.putStrLn "Type a new repeat count [1..5]: "
  _ -> pure ()
