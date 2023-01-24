module Handlers.Client where
-- тут реализация консольной версии
import Types (LastMessage, Message)
import qualified Handlers.Logger as HL

data Handle m = Handle
  { fetch :: Maybe LastMessage -> m (Maybe Message)
  , carryAway :: Message -> m ()
  , logger :: HL.Handle m  
  }


