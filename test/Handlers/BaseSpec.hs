module Handlers.BaseSpec (spec) where

import Config (Config (..))
import Control.Monad (void)
import Control.Monad.State (State , evalState,  get, modify)
import qualified Data.Map.Strict as Map (Map, fromList, insert, lookup)
import qualified Data.Text as T
import qualified Handlers.Logger (Handle (..))
import Test.Hspec (Spec,    it, shouldBe)
import Types (     Log (..),  RepeatCount (..), User (..))

import Handlers.Base

spec :: Spec
spec = do

    let testUser = User 341 :: User
    let newUser = User 11 :: User
    let repeatCount = RepeatCount 2 :: RepeatCount
    let testBase = Map.fromList [(testUser, repeatCount)]
    let logerHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            } ::
            Handlers.Logger.Handle (State (Map.Map User RepeatCount))
    let baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logerHandle,
              Handlers.Base.defaultRepeatCount = RepeatCount $ cRepeatCount testConfig,
              Handlers.Base.findUser = \user -> get >>= \base -> pure $ Map.lookup user base,
              Handlers.Base.updateUser = \user count -> void $ modify (Map.insert user count)
            }
    it "return repeat count from Base for user" $
      evalState (Handlers.Base.giveRepeatCountFromBase baseHandle testUser) testBase
        `shouldBe` repeatCount

    it "return default repeat count from Base for new user" $
      evalState (Handlers.Base.giveRepeatCountFromBase baseHandle newUser) testBase
        `shouldBe` RepeatCount (cRepeatCount testConfig)

testConfig :: Config
testConfig =
  Config
    { cRepeatCount = 3,
      cTextMenuHelp = "Help menu:" :: T.Text,
      cTextMenuRepeat = "Repeat menu:" :: T.Text,
      cLvlLog = Debug :: Log
    }
