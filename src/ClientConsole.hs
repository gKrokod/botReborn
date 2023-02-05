module ClientConsole where

-- тут реализация консольной версии
import Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.System as Time

fetch :: Maybe LastMessage -> IO (Maybe Message)
fetch lm = do
  m <- getLine
  time <- Time.getSystemTime
  let msg = Message {mID = fromIntegral $ Time.systemSeconds time, mUser = 1}
  case lm of
    Nothing -> pure $ Just $ makeMessage m msg
    Just lm -> if mID lm == mID msg
               then pure Nothing
	       else pure $ Just $ makeMessage m msg

makeMessage :: String -> Message -> Message
makeMessage t msg = case t of
  "/help"   -> msg {mData = Command "/help"}
  "/repeat" -> msg {mData = Command "/repeat"}
  otherwise -> msg {mData = Msg $ T.pack t}


carryAway :: Message -> IO ()
carryAway msg = case mData msg of
                    Query i      -> print i
		    Msg t        -> do 
		                      TIO.putStrLn t
		    KeyboardMenu -> TIO.putStrLn ("Type a new repeat count [1..5]: ")
		    otherwise    -> pure ()
