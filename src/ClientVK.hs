
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



fetch :: Maybe LastMessage -> IO (Maybe Message)
fetch lm = undefined

carryAway :: Message -> IO ()
carryAway = undefined

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

