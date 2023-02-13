module Handlers.Client where

-- тут реализация консольной версии

import qualified Handlers.Logger
import Types (Config, LastMessage, Message)

data Handle m = Handle
  { fetch :: Maybe LastMessage -> m (Maybe Message),
    carryAway :: Message -> m (),
    logger :: Handlers.Logger.Handle m
  }
