module ClientConsole (fetch, carryAway) where

-- тут реализация консольной версии

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.System as Time
import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime, localDay)
import Data.Time.Calendar (Day, cdMonths, diffGregorianDurationClip)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Types (Data (..), LastMessage, Message (..), defaultMessage)

fetch :: Maybe LastMessage -> IO (Maybe Message)
fetch lm = do
  m <- getLine
  time <- Time.getSystemTime
  today <- localDay <$> zonedTimeToLocalTime <$> getZonedTime  
  let msg = defaultMessage {mID = fromIntegral $ Time.systemSeconds time} 
  case isCalendar m of
    Nothing -> do
      case lm of
        Nothing -> pure $ Just $ makeMessage m msg
        Just m' ->
          if mID m' == mID msg
            then pure Nothing
            else pure $ Just $ makeMessage m msg
    Just day -> do
      let resultYear = cdMonths (diffGregorianDurationClip today day) `div` 12
      pure $ Just $ msg {mData = Calendar $ T.pack $ show resultYear}

makeMessage :: String -> Message -> Message
makeMessage t msg = case t of
  "/help" -> msg {mData = Command "/help"}
  "/start" -> msg {mData = Command "/help"}
  "/repeat" -> msg {mData = Command "/repeat"}
  _ -> msg {mData = Msg $ T.pack t}

isCalendar :: String -> Maybe Day
isCalendar = parseTimeM True defaultTimeLocale "%Y-%-m-%-d"

carryAway :: Message -> IO ()
carryAway msg = case mData msg of
  -- Query i      -> print i
  Msg t -> TIO.putStrLn t
  KeyboardMenu -> TIO.putStrLn ("Type a new repeat count [1..5]: ")
  Calendar t -> TIO.putStrLn t
  _ -> pure ()

-- import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime, localDay)
-- import Data.Time.Calendar (Day, cdMonths, diffGregorianDurationClip)
-- import Data.Time.Format (parseTimeM, defaultTimeLocale)
-- --  localDay <$> zonedTimeToLocalTime <$>  getZonedTime  :: IO Day
-- -- parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04" :: Maybe Day
--
--
-- main :: IO ()
-- main = do
--   msg <- getLine
--   let parse = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" msg :: Maybe Day
--   nowday <- localDay <$> zonedTimeToLocalTime <$>  getZonedTime  
--   case parse of
--     Nothing -> pure ()
--     Just x -> print (cdMonths (diffGregorianDurationClip nowday x) `div` 12)

