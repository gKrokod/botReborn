module ClientVK where

-- тут реализация vk версии
import Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.System as Time
import ClientVK.HttpMessage
import ClientVK.Parse
import Network.HTTP.Simple --(parseRequest, Request, httpLBS, getResponseBody, getResponseStatusCode, getResponseHeader)
import qualified Data.Text.Encoding as E (encodeUtf8)
import qualified Data.ByteString.Char8 as BC (pack)
import Data.Aeson 
import Control.Monad
import Data.Function ((&))


fetch :: Config -> Maybe LastMessage -> IO (Maybe Message)
fetch c lm = do
  let cfg = c {cOffset = maybe "-1" (BC.pack . show . succ . mID) lm } 
  response <- httpLBS $ buildGetRequest cfg
  let status = getResponseStatusCode response
  if (404 == status || status == 301) then pure Nothing
  else do
    let msg = decode $ getResponseBody $ response
    case msg of
      Nothing -> pure Nothing
      Just m -> pure $ Just $ makeMessage (mData m) m
  -- let status = getResponseStatusCode response
  -- case status of
  --   404 -> Handlers.Logger.logMessage logHandle Fatal "Bot Server not found"
  --   301 -> Handlers.Logger.logMessage logHandle Fatal "Bot Server Moved Permanently"
  --   200 -> do  
  --     Handlers.Logger.logMessage logHandle Debug "Bot Server give us response"
  --     print $ getResponseHeader "Content-Type" response
  --     print $ getResponseBody response
  -- let jsonBody = getResponseBody response
  -- let mbMessage = decode jsonBody :: Maybe Message
  -- pure $ decode $ getResponseBody $ response
  
makeMessage :: Data T.Text DataFromButton -> Message -> Message
makeMessage (Msg t) msg = case t of
  "/help"   -> msg {mData = Command "/help"}
  "/repeat" -> msg {mData = Command "/repeat"}
  otherwise -> msg 
makeMessage _ msg = msg

carryAway :: Config -> Message -> IO ()
carryAway cfg msg = case mData msg of
                      Msg t -> httpLBS (buildTextSendRequest cfg msg) >> pure ()
		      Gif t -> httpLBS (buildGifSendRequest cfg msg) >> pure ()
		      KeyboardMenu -> httpLBS (buildKeyboardSendRequest cfg msg) >> pure ()
		      otherwise -> pure ()

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
    setRequestQueryString ([("chat_id", msg & Just . BC.pack . show . mUser), ("text", Just $ E.encodeUtf8 $ "opa keyboard"), ("reply_markup", justKeyBoard)])
  $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, "/sendMessage"])
  $ buildDefaultSendRequest cfg
  
-- data Data t i = Msg t | Gif t | Command t | KeyboardMenu | Query i deriving (Show, Eq)
--
-- data Message = Message 
--   {
--     mData :: Data T.Text DataFromButton 
--   , mID :: ID
--   , mUser :: User
--   } deriving (Eq, Show) --show for test dispatcher
        -- let echoMessage = replicate numberCount (buildSendRequest (cfg) ourAnswer)
        -- botResponse' <- mapM httpLBS echoMessage
-- buildSendRequest :: Config -> Message -> Request
-- buildSendRequest cfg (Message msg) =
--     setRequestHost (cfg & cBotHost)
--   $ setRequestMethod (cfg & cMethod)
--   $ setRequestSecure (cfg & cSecure)
--   $ setRequestQueryString ([("chat_id", msg & Just . BC.pack . show . mUser ), queryMsg $ msg & tMessage, ("reply_markup", user & keyboardMenu)  ])
--   $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, either (\_ -> "/sendMessage") (\_ -> "/sendAnimation") (user & tMessage) ])
--   $ setRequestPort (cfg & cPort)
--   $ defaultRequest
--     where queryMsg (Left msg1) = ("text", Just $ E.encodeUtf8 msg)
--           queryMsg (Right msg2) = ("animation", Just $ E.encodeUtf8 msg)

-- buildDefaultSendRequest :: Config -> Request
-- buildDefaultSendRequest cfg =
--     setRequestHost (cfg & cBotHost)
--   $ setRequestMethod (cfg & cMethod)
--   $ setRequestSecure (cfg & cSecure)
--   $ setRequestPort (cfg & cPort)
--   $ defaultRequest
--
--
--
--
-- carryAway :: Message -> IO ()
-- carryAway msg = case mData msg of
--                     Query i      -> print i
--                     Msg t        -> do 
--                                       TIO.putStrLn t
--                     KeyboardMenu -> TIO.putStrLn ("Type a new repeat count [1..5]: ")
--                     otherwise    -> pure ()
--
--
-- makeMessage :: String -> Message -> Maybe Message
-- makeMessage t msg = case t of
--   "/help"   -> Just $ msg {mData = Command "/help"}
--   "/repeat" -> Just $ msg {mData = Command "/repeat"}
--   otherwise -> Just $ msg {mData = Msg $ T.pack t}

