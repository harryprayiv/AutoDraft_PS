module Sorting where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import Player (Player, PlayersMap(..))

type SortOption = String

sortOptions :: Array SortOption
sortOptions = ["ID", "Surname", "'23 Rank", "'23 Points"]

-- Convert PlayersMap to a sortable list
toSortableList :: PlayersMap -> List (Tuple String Player)
toSortableList (PlayersMap playersMap) = Map.toUnfoldable playersMap

-- Convert the sorted list back to PlayersMap
fromSortableList :: List (Tuple String Player) -> PlayersMap
fromSortableList = PlayersMap <<< Map.fromFoldable

-- Enhanced sorting function to handle Maybe values
sortPlayersBy :: forall a. Ord a => (Player -> Maybe a) -> List (Tuple String Player) -> List (Tuple String Player)
sortPlayersBy f playersList =
  List.sortBy (\x y -> compareMaybes (f (snd x)) (f (snd y))) playersList

-- Helper function to define how Maybe values are compared
compareMaybes :: forall a. Ord a => Maybe a -> Maybe a -> Ordering
compareMaybes Nothing Nothing = EQ
compareMaybes Nothing (Just _) = LT
compareMaybes (Just _) Nothing = GT
compareMaybes (Just a) (Just b) = compare a b

-- Updated sortBySelectedOption function
sortBySelectedOption :: SortOption -> PlayersMap -> PlayersMap
sortBySelectedOption sortOption playersMap =
  let
    playersList = toSortableList playersMap
    sortedList = case sortOption of
      "ID" -> sortPlayersBy (Just <<< _.playerId) playersList
      "Surname" -> sortPlayersBy (Just <<< _.useLastName) playersList
      "'23 Rank" -> sortPlayersBy _.past_ranking playersList
      "'23 Points" -> sortPlayersBy _.past_fpts playersList
      _ -> playersList
  in
    fromSortableList sortedList
