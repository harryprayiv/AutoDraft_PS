module Filtering
  ( filterActivePlayers
  , toggleFilter
  )
  where

import Prelude

import Data.Array (elem, (\\))
import Data.Map (Map)
import Data.Map as Map
import Player (Player)

toggleFilter :: forall a. Eq a => a -> Array a -> Array a
toggleFilter x arr = if elem x arr then arr \\ [x] else arr <> [x]

filterActivePlayers :: Array String -> Map String Player -> Map String Player
filterActivePlayers posCodes playersMap =
  case posCodes of
    [] -> playersMap 
    codes -> Map.filter (\player -> elem player.primaryPosition codes) playersMap