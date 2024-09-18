module Clients.Telegram.Internal.CarryAway (carryAway) where

import Clients.Telegram.Internal.HttpMessage
  ( buildGifSendRequest,
    buildKeyboardSendRequest,
    buildTextSendRequest,
  )
import Config (Config (..))
import Control.Monad (void)
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple
  ( httpLBS,
  )
import Types (Data (..), Message (..))

carryAway :: Config -> Message -> IO ()
carryAway cfg msg = case mData msg of
  Msg _ -> void $ httpLBS (buildTextSendRequest cfg msg)
  Gif _ -> void $ httpLBS (buildGifSendRequest cfg msg)
  KeyboardMenu -> void $ httpLBS (buildKeyboardSendRequest cfg msg)
  _ -> void $ TIO.putStrLn "carry away wrong message"
