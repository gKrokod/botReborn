module ClientConsole where

-- тут реализация консольной версии
import Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.System as Time


fetch :: IO (Maybe Message)
fetch = do
  m <- getLine
  time <- Time.getSystemTime
  let msg = Message { mID = fromIntegral $ Time.systemSeconds time, mUser = 1}
  case m of
    "/help"   -> pure $ Just $ msg {mData = Command "/help"}
    "/repeat" -> pure $ Just $ msg {mData = Command "/repeat"}
    otherwise -> pure $ Just $ msg {mData = Msg $ T.pack m}

carryAway :: Message -> IO ()
carryAway msg = case mData msg of
                    Query i      -> print i
		    Msg t        -> TIO.putStrLn t
		    KeyboardMenu -> TIO.putStrLn ("Type a new repeat count [1..5]: ")
		    otherwise    -> pure ()
