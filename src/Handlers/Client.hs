module Handlers.Client where
-- тут реализация консольной версии
import Types (LastMessage, Message)

data Handle m = Handle
  { fetch :: Maybe LastMessage -> m (Maybe Message)
  , carryAway :: Message -> m ()
  }


