module Handlers.Client where
-- тут реализация консольной версии
import Types (LastMessage, Message, Config)
import qualified Handlers.Logger

data Handle m = Handle
  { fetch :: Maybe LastMessage -> m (Maybe Message)
  , carryAway :: Message -> m ()
  , logger :: Handlers.Logger.Handle m  
  }


