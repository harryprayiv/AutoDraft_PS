module Sorting where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Player (Player)

type SortOption = String

sortOptions :: Array SortOption
sortOptions = ["ID", "Surname", "'23 Rank", "'23 Points"]

sortBySelectedOption :: SortOption -> Array (Tuple String Player) -> Array (Tuple String Player)
sortBySelectedOption sortOption playersArray =
  case sortOption of
    "ID" -> sortPlayersBy (\player -> Just player.playerId) playersArray
    "Surname" -> sortPlayersBy (\player -> Just player.useLastName) playersArray
    "'23 Rank" -> sortPlayersBy _.past_ranking playersArray
    "'23 Points" -> sortPlayersBy _.past_fpts playersArray
    _ -> playersArray

sortPlayersBy :: forall a. Ord a => (Player -> Maybe a) -> Array (Tuple String Player) -> Array (Tuple String Player)
sortPlayersBy f playersArray =
  let
    comparePlayers (Tuple _ p1) (Tuple _ p2) = compareMaybe (f p1) (f p2)
    compareMaybe Nothing Nothing   = EQ
    compareMaybe Nothing (Just _)  = GT
    compareMaybe (Just _) Nothing  = LT
    compareMaybe (Just a) (Just b) = compare a b
  in
    Array.sortBy comparePlayers playersArray
