module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Handlers.Dispatcher (dispatcher, watcherForNewMessage)
import Initialization (initialization)

main :: IO ()
main = do
  handle <- initialization
  --  run watcher for new messages
  void . forkIO . forever . watcherForNewMessage $ handle
  --  run main logic
  forever . dispatcher $ handle
