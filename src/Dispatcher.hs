module Dispatcher where
import Types (User)
import Base (UserDataBase)
import Control.Concurrent (forkIO)
import Control.Monad (forever)

forkForUser :: IO () -> IO ()
forkForUser f = do
  forkIO $ forever f
  -- print "****************************MAKE NEW FORK****************************************"
  pure ()
