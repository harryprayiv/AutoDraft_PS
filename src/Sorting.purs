module Sorting where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import Player (Player)

type SortOption = String

sortOptions :: Array SortOption
sortOptions = ["ID", "Surname", "'23 Rank", "'23 Points"]

sortBySelectedOption :: SortOption -> Map String Player -> Map String Player
sortBySelectedOption sortOption playersMap =
  let
    playersList = Map.toUnfoldable playersMap
  in
    case sortOption of
      "ID" -> sortPlayersBy (\player -> Just player.playerId) playersList
      "Surname" -> sortPlayersBy (\player -> Just player.useLastName) playersList
      "'23 Rank" -> sortPlayersBy _.past_ranking playersList
      "'23 Points" -> sortPlayersBy _.past_fpts playersList
      _ -> playersList
  # Map.fromFoldable

sortPlayersBy :: forall a. Ord a => (Player -> Maybe a) -> List (Tuple String Player) -> List (Tuple String Player)
sortPlayersBy f playersList =
  List.sortBy (comparing (f <<< snd)) playersList
