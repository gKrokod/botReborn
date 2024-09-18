module Clients.Console.Internal.CarryAway (carryAway) where

import qualified Data.Text.IO as TIO
import Types (Data (..), Message (..))

carryAway :: Message -> IO ()
carryAway msg = case mData msg of
  Msg t -> TIO.putStrLn t
  KeyboardMenu -> TIO.putStrLn "Type a new repeat count [1..5]: "
  _ -> pure ()
