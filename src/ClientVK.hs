module ClientVK where

-- module Handlers.Client where
-- -- тут реализация консольной версии
-- import Types (LastMessage, Message)
-- import qualified Handlers.Logger
--
-- data Handle m = Handle
--   { fetch :: Maybe LastMessage -> m (Maybe Message)
--   , carryAway :: Message -> m ()
--   , logger :: Handlers.Logger.Handle m  
--   }
--

-- тут реализация vk версии
import Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.System as Time
import ClientVK.HttpMessage
import Network.HTTP.Simple --(parseRequest, Request, httpLBS, getResponseBody, getResponseStatusCode, getResponseHeader)
import qualified Data.Text.Encoding as E (encodeUtf8)
import qualified Data.ByteString.Char8 as BC (pack)
import Data.Aeson 


fetch :: Config -> Maybe LastMessage -> IO (Maybe Message)
fetch c lm = do
  let cfg = c {cOffset = maybe "-1" (BC.pack . show . succ . mID) lm } 
  response <- httpLBS $ buildGetRequest cfg
  -- let status = getResponseStatusCode response
  -- case status of
  --   404 -> Handlers.Logger.logMessage logHandle Fatal "Bot Server not found"
  --   301 -> Handlers.Logger.logMessage logHandle Fatal "Bot Server Moved Permanently"
  --   200 -> do  
  --     Handlers.Logger.logMessage logHandle Debug "Bot Server give us response"
  --     print $ getResponseHeader "Content-Type" response
  --     print $ getResponseBody response
  let jsonBody = getResponseBody response
  let mbMessage = decode jsonBody :: Maybe Message
  pure mbMessage

carryAway :: Config -> Message -> IO ()
carryAway = undefined

  -- print $ buildGetRequest (cfg)
  -- response <- httpLBS $ buildGetRequest (cfg)
  -- let status = getResponseStatusCode response
  -- case status of
  --   404 -> Handlers.Logger.logMessage logHandle Fatal "Bot Server not found"
  --   301 -> Handlers.Logger.logMessage logHandle Fatal "Bot Server Moved Permanently"
  --   200 -> do  
  --     Handlers.Logger.logMessage logHandle Debug "Bot Server give us response"
  --     print $ getResponseHeader "Content-Type" response
  --     print $ getResponseBody response
  --     let jsonBody = getResponseBody (response)
  --     let mbMessage = decode jsonBody :: Maybe Message
  --     case mbMessage of
  --       Just m -> do
	--   Handlers.Logger.logMessage logHandle Debug "Get message"
	--   print m
  --         -- LC.putStrLn jsonBody
  --         L.writeFile "data.json" jsonBody
	-- Nothing -> do 
	--   print "nothing"
  --         let mbMessage = eitherDecode jsonBody :: Either String Message
	--   print $ mbMessage

-- fetch :: Maybe LastMessage -> IO (Maybe Message)
-- fetch lm = do
--   m <- getLine
--   time <- Time.getSystemTime
--   let msg = Message {mID = fromIntegral $ Time.systemSeconds time, mUser = 1}
--   case lm of
--     Nothing -> pure $ makeMessage m msg
--     Just lm -> if mID lm == mID msg
--                then pure Nothing
--                else pure $ makeMessage m msg
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

