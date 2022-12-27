module Bot where
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
    "/help" -> pure $ msg {mData = Command commandHelp}
    "/repeat" -> pure $ msg {mData = Command commandRepeat}
    _ -> pure $ msg {mData = Msg $ T.pack m}

defaultRepeatCount :: RepeatCount
defaultRepeatCount = 3

updateUser :: User -> RepeatCount -> IO ()
updateUser user count = pure ()

findUser :: User -> IO (Maybe RepeatCount)
findUser user = pure $ Just (3 :: RepeatCount)

sendMessage :: Message -> IO ()
sendMessage msg = case mData msg of
                    Query i -> print i
		    Command c -> case c of
		                   "/help" -> TIO.putStrLn ("help menu")
		                   "/repeat" -> TIO.putStrLn ("repeat menu")
		    Msg m -> TIO.putStrLn m
