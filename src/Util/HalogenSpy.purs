module Util.HalogenSpy where

-- import Prelude
-- import DraftComponent

-- import Data.Array
-- import Data.Foldable (foldMap)
-- import Data.Map as Map
-- import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Show (show)
-- import Effect.Console as CONSOLE
-- import Types.Player (PlayersMap(..), Player)
-- import Data.Tuple

-- showPlayer :: String -> Player -> String
-- showPlayer key player = "{ Key: " <> key <> ", Player: " <> show player <> " }"

-- showPlayersMap :: PlayersMap -> String
-- showPlayersMap (PlayersMap playersMap) =
--   "[" <> (foldMap (uncurry showPlayer) $ Map.toUnfoldable playersMap) <> "]"


-- spyHalogenState :: forall state. Show state => String -> State -> State
-- spyHalogenState msg state = 
--   CONSOLE.log (msg <> ": " <> show state)
--   state              