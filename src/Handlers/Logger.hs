module Handlers.Logger where
import Types (Log)
import qualified Data.Text as T

data Handle m = Handle 
  {  levelLogger :: Log
  ,  writeLog :: T.Text -> m ()
  }

logMessage :: (Monad m) => Handle m -> Log -> T.Text -> m ()
logMessage h lvl msg 
  | lvl >= (levelLogger h) = writeLog h (mconcat["[",T.pack $ show lvl,"] ", msg])
  | otherwise = pure ()
