module Handlers.Dispatcher where
import qualified Handlers.Base
import qualified Handlers.Client
import Types

data Handle m = Handle
  { readMessage :: m (Maybe Message, Maybe LastMessage)
  , saveMessage :: Message -> m ()
  , eraseMessage :: Message -> m ()
  , client :: Handlers.Client.Handle m
  , base :: Handlers.Base.Handle m
  }


getMessage :: (Monad m) => Handle m -> m (Message)
getMessage h = undefined

-- (stack, lastMsg) <- readMessage
-- case stack of
--   Nothing -> do
--     msg <- Handlers.Client.fetch lastMsg
--     case msg of
--       Nothing -> (do error "can't fetch"; getMessage)
--       Just msg -> do
--         saveMessage msg
-- 	pure msg


makeForkForUser :: (Monad m) => Handle m -> m (Message)
makeForkForUser = undefined


