module Logger where
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import qualified Handlers.Logger

writeLog :: T.Text -> IO ()
writeLog = TIO.putStrLn 
-- writeLog = TIO.appendFile "Log.txt"
