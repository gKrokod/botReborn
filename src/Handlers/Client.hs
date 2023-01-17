module Handlers.Client where
-- тут реализация консольной версии
import Types

data Handle m = Handle
  { fetch :: m (Maybe Message)
  , carryAway :: Message -> m ()
  }


