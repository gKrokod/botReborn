module Dispatcher where
import Types (User)
import Base (UserDataBase)
import Control.Concurrent
import Control.Monad

forkForUser :: IO () -> IO ()
forkForUser f = do
  forkIO $ forever f
  print "****************************MAKE NEW FORK****************************************"
