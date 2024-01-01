module Filtering
  ( filterActivePlayers
  , toggleFilter
  )
  where

import Prelude

import Data.Array (elem, (\\))
import Data.Map as Map
import Types.Player (PlayersMap(..))

toggleFilter :: forall a. Eq a => a -> Array a -> Array a
toggleFilter x arr = if elem x arr then arr \\ [x] else arr <> [x]

-- Filtering.purs
filterActivePlayers :: Array String -> PlayersMap -> PlayersMap
filterActivePlayers posCodes (PlayersMap playersMap) =
  case posCodes of
    [] -> PlayersMap playersMap
    codes -> PlayersMap $ Map.filter (\player -> elem player.primaryPosition codes) playersMap
