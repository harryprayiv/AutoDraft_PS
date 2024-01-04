module Mutation
  ( fetchPlayers
  , fetchRankings
  , filterActivePlayers
  , fromSortableList
  , mergeAndSortPlayers
  , mergePlayerData
  , sortBySelectedOption
  , sortDisplayPlayers
  , sortDisplayPlayersBy
  , sortPlayersBy
  , toSortableList
  , toggleFilter
  )
  where

import Prelude

import Affjax (defaultRequest, printError)
import Affjax.ResponseFormat (json, string)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (elem, (\\))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Int as DI
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Number as DN
import Data.String (trim)
import Data.Tuple (Tuple, snd)
import Effect.Aff (Aff)
import Types.Player (ActivePlayers(..), DisplayPlayer, DisplayPlayers, Player, PlayersMap(..), RankingCSV, RequestFunction, SortOption, SortValue(..), compareMaybes, parseRankingCSV, transformToDisplayPlayers)

-- Fetching
fetchPlayers :: RequestFunction -> Aff (Either String PlayersMap)
fetchPlayers requestFunction = do
  response <- requestFunction $ defaultRequest
    { url = "./testData/fake_Players.json"
    , responseFormat = json
    }
  case response of
    Left err -> do
      let errorMsg = "Fetch Error: " <> printError err
      pure $ Left errorMsg

    Right res -> do
      case decodeJson res.body of
        Right (ActivePlayers playersMap) -> pure $ Right playersMap
        Left decodeError -> do
          let errorMsg = "Decode Error: " <> printJsonDecodeError decodeError
          pure $ Left errorMsg

fetchRankings :: RequestFunction -> Aff (Either String RankingCSV)
fetchRankings requestFunction = do
  response <- requestFunction $ defaultRequest 
    { url = "./testData//fake_2023_Rankings.csv"
    , responseFormat = string 
    }
  case response of
    Left err -> pure $ Left $ printError err
    Right res -> pure $ Right $ parseRankingCSV res.body

mergeAndSortPlayers :: PlayersMap -> RankingCSV -> SortOption -> DisplayPlayers
mergeAndSortPlayers playersMap rankings sortOption =
  let
    mergedPlayersMap = mergePlayerData playersMap rankings
    displayPlayers = transformToDisplayPlayers mergedPlayersMap
    sortedDisplayPlayers = sortDisplayPlayers sortOption true displayPlayers
  in
    sortedDisplayPlayers

mergePlayerData :: PlayersMap -> RankingCSV -> PlayersMap
mergePlayerData (PlayersMap playersMap) csvData = PlayersMap $ Array.foldl updatePlayerRanking playersMap csvData
  where
    updatePlayerRanking acc row = case row of
      [mlbId, _, _, fptsStr, rankingStr] ->
        let
          maybeRanking = DI.fromString $ trim rankingStr
          maybeFPTS = DN.fromString $ trim fptsStr
        in Map.update (updatePlayer maybeRanking maybeFPTS) mlbId acc
      _ -> acc

    updatePlayer :: Maybe Int -> Maybe Number -> Player -> Maybe Player
    updatePlayer maybeRanking maybeFPTS player = Just $ player
      { past_ranking = if isNothing maybeRanking then player.past_ranking else maybeRanking
      , past_fpts = if isNothing maybeFPTS then player.past_fpts else maybeFPTS }

-- Filtering
toggleFilter :: forall a. Eq a => a -> Array a -> Array a
toggleFilter x arr = if elem x arr then arr \\ [x] else arr <> [x]

filterActivePlayers :: Array String -> PlayersMap -> PlayersMap
filterActivePlayers posCodes (PlayersMap playersMap) =
  case posCodes of
    [] -> PlayersMap playersMap
    codes -> PlayersMap $ Map.filter (\player -> elem player.primaryPosition codes) playersMap

-- Sorting
toSortableList :: PlayersMap -> List (Tuple String Player)
toSortableList (PlayersMap playersMap) = Map.toUnfoldable playersMap

fromSortableList :: List (Tuple String Player) -> PlayersMap
fromSortableList = PlayersMap <<< Map.fromFoldable

sortPlayersBy :: forall a. Ord a => Boolean -> (Player -> Maybe a) -> List (Tuple String Player) -> List (Tuple String Player)
sortPlayersBy sortOrder f playersList =
  let
    comparison = if sortOrder then compareMaybes else flip compareMaybes
  in
    List.sortBy (\x y -> comparison (f (snd x)) (f (snd y))) playersList

sortDisplayPlayersBy :: Boolean -> (DisplayPlayer -> SortValue) -> DisplayPlayers -> DisplayPlayers
sortDisplayPlayersBy sortOrder f playersArray =
  let
    comparison = if sortOrder then compare else flip compare
  in
    Array.sortBy (\x y -> comparison (f x) (f y)) playersArray

sortDisplayPlayers :: SortOption -> Boolean -> DisplayPlayers -> DisplayPlayers
sortDisplayPlayers sortOption sortOrder players =
  case sortOption of
    "ID" -> sortDisplayPlayersBy sortOrder (SortString <<< _.id) players
    "Surname" -> sortDisplayPlayersBy sortOrder (SortString <<< _.useLastName) players
    "'23 Rank" -> sortDisplayPlayersBy sortOrder (SortNumber <<< map toNumber <<< _.past_ranking) players
    "'23 Points" -> sortDisplayPlayersBy sortOrder (SortNumber <<< _.past_fpts) players
    _ -> players
 
sortBySelectedOption :: SortOption -> Boolean -> PlayersMap -> PlayersMap
sortBySelectedOption sortOption sortOrder playersMap =
  let
    playersList = toSortableList playersMap
    sortedList = case sortOption of
      "ID" -> sortPlayersBy sortOrder (Just <<< _.playerId) playersList
      "Surname" -> sortPlayersBy sortOrder (Just <<< _.useLastName) playersList
      "'23 Rank" -> sortPlayersBy sortOrder _.past_ranking playersList
      "'23 Points" -> sortPlayersBy sortOrder _.past_fpts playersList
      _ -> playersList
  in
    fromSortableList sortedList