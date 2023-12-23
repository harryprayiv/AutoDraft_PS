module Sorting where

import Prelude

import Data.Array (sortBy)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Player (Player)

type SortOption = String

sortOptions :: Array SortOption
sortOptions = ["ID", "Surname", "'23 Rank", "'23 Points"]

sortBySelectedOption :: SortOption -> Map String Player -> Map String Player
sortBySelectedOption sortOption playersMap = 
  case sortOption of
    "ID" -> sortByid playersMap
    "Surname" -> sortBySurname playersMap
    "'23 Rank" -> sortByRank playersMap
    "'23 Points" -> sortByPoints playersMap
    _ -> playersMap

sortByid :: Map String Player -> Map String Player
sortByid = sortPlayersBy (\player -> Just player.playerId)

sortBySurname :: Map String Player -> Map String Player
sortBySurname = sortPlayersBy (\player -> Just (player.useLastName <> " " <> player.useName))

sortByRank :: Map String Player -> Map String Player
sortByRank = sortPlayersBy (\player -> player.past_ranking)

sortByPoints :: Map String Player -> Map String Player
sortByPoints = sortPlayersBy (\player -> player.past_fpts)

sortPlayersBy :: forall a. Ord a => (Player -> Maybe a) -> Map String Player -> Map String Player
sortPlayersBy f playersMap =
  let
    comparePlayers t1 t2 = compareMaybe (f $ snd t1) (f $ snd t2)
    compareMaybe Nothing Nothing   = EQ
    compareMaybe Nothing (Just _)  = GT
    compareMaybe (Just _) Nothing  = LT
    compareMaybe (Just a) (Just b) = compare a b
  in
    Map.fromFoldable $ sortBy comparePlayers $ Map.toUnfoldable playersMap