module ConsoleBot where
-- тут реализация консольной версии
import qualified Handlers.Bot as Handler
import Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


getMessage :: IO (Message)
getMessage = do
  m <- getLine
  let msg = Message { mID = 1, mUser = 1}
  case m of
    "/help"   -> pure $ msg {mData = Command "/help"}
    "/repeat" -> pure $ msg {mData = Command "/repeat"}
    otherwise -> pure $ msg {mData = Msg $ T.pack m}

sendMessage :: Message -> IO ()
sendMessage msg = case mData msg of
                    Query i      -> print i
		    Msg t        -> TIO.putStrLn t
		    KeyboardMenu -> TIO.putStrLn ("Type a new repeat count [1..5]: ")
		    otherwise    -> pure ()
