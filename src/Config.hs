{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module Config (loadConfig, Config (..), Mode (..)) where

import Control.Exception (SomeException, displayException, throwIO, try)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import GHC.Generics (Generic)
import Types (Log (..))

data Config = Config
  { cRepeatCount :: Int,
    cTextMenuHelp :: Text,
    cTextMenuRepeat :: Text,
    cApiPath :: Text,
    cBotHost :: Text,
    cTimeOut :: Text,
    cOffset :: Text,
    cToken :: Text,
    cPort :: Int,
    cMethod :: Text,
    cSecure :: Bool,
    cMode :: Mode,
    cLvlLog :: Log
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Mode = ConsoleBot | TelegramBot
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

loadConfig :: IO Config
loadConfig = do
  cfg <- loadConfigBot
  case cfg of
    Left error' -> throwIO $ userError error'
    Right config -> pure config

--
loadConfigBot :: IO (Either String Config)
loadConfigBot =
  either (Left . displayException) eitherDecode
    <$> try @SomeException (L.readFile "config/bot.cfg")
