module Logger (writeLog) where

import qualified Data.Text as T (Text)
import qualified Data.Text.IO as TIO

writeLog :: T.Text -> IO ()
writeLog = TIO.putStrLn

-- logMessage :: (Monad m) => Handle m -> Log -> T.Text -> m ()
-- logMessage h lvl msg
--   | lvl >= levelLogger h = writeLog h (mconcat ["[", T.pack $ show lvl, "] ", msg])
--   | otherwise = pure ()
