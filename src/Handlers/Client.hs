module Handlers.Client (Handle (..)) where

-- тут реализация консольной версии

import qualified Handlers.Logger
import Types (LastMessage, Message)

data Handle m = Handle
  { fetch :: Maybe LastMessage -> m (Maybe Message),
    carryAway :: Message -> m (),
    logger :: Handlers.Logger.Handle m
  }
