module Handlers.Client where
-- тут реализация консольной версии
import Types

data Handle m = Handle
  { fetch :: Maybe LastMessage -> m (Maybe Message)
  , carryAway :: Message -> m ()
  }


