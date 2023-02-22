module ClientTM (fetch, carryAway) where

-- тут реализация vk версии

import ClientTM.HttpMessage
  ( buildGetRequest,
    buildGifSendRequest,
    buildKeyboardSendRequest,
    buildTextSendRequest,
  )
import ClientTM.Parse (BoxMessage (..), UnknownMessage (..))
import Control.Exception 
-- import Control.Exception.Base
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseStatusCode,
    httpLBS,
  )
import Types (Config (..), Data (..), DataFromButton, LastMessage, Message (..), defaultMessage)
import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime, localDay)
import Data.Time.Calendar (Day, cdMonths, diffGregorianDurationClip)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

fetch :: Config -> Maybe LastMessage -> IO (Maybe Message)
fetch cfg lm = do
  today <- localDay <$> zonedTimeToLocalTime <$> getZonedTime  
  response' <- try (httpLBS $ buildGetRequest (cfg {cOffset = maybe "-1" (BC.pack . show . succ . mID) lm}))
  case response' of
    Left e -> do
      print (e :: SomeException)
      threadDelay (10)
      fetch cfg lm
    Right response -> do
      let status = getResponseStatusCode response
      when (404 == status || status == 301) (TIO.putStrLn "Error! Bot Server 404 or 301")
      let msg = decode $ getResponseBody $ response -- messages : text, gif
      let umsg = decode $ getResponseBody $ response -- another messages
      case (msg, umsg) of
        (Just m, _) -> do
          let m' = unboxMessage m
          case isCalendar (mData m') of
            Nothing -> pure $ Just $ makeMessage (mData  m') (m')
            Just day -> do 
              let resultYear = cdMonths (diffGregorianDurationClip today day) `div` 12
              pure $ Just $ m' {mData = Calendar $ T.pack $ show resultYear}
        (_, Just m) -> fetch cfg (Just defaultMessage {mID = uID m})
        _ -> pure Nothing


makeMessage :: Data T.Text DataFromButton -> Message -> Message
makeMessage (Msg t) msg = case t of
  "/help" -> msg {mData = Command "/help"}
  "/start" -> msg {mData = Command "/help"}
  "/repeat" -> msg {mData = Command "/repeat"}
  _ -> msg
makeMessage _ msg = msg

isCalendar :: Data T.Text DataFromButton -> Maybe Day
isCalendar (Msg t) = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (T.unpack t)
isCalendar _ = Nothing 

carryAway :: Config -> Message -> IO ()
carryAway cfg msg = case mData msg of
  Msg _ -> httpLBS (buildTextSendRequest cfg msg) >> pure ()
  Gif _ -> httpLBS (buildGifSendRequest cfg msg) >> pure ()
  KeyboardMenu -> httpLBS (buildKeyboardSendRequest cfg msg) >> pure ()
  Calendar _ -> httpLBS (buildTextSendRequest cfg msg) >> pure ()
  _ -> TIO.putStrLn "carryAway wrong message" >> pure ()
