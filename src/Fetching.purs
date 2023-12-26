module Fetching
  ( fetchPlayers
  , fetchRankings
  , mergeAndSortPlayers
  , mergePlayerData
  )
  where

import Prelude

import Affjax (defaultRequest)
import Affjax.ResponseFormat (json, string)
import Affjax.Web as AW
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Either (Either(..))
import Data.Map (Map)
import Effect.Aff (Aff)
import Player (ActivePlayers(..), Player, PlayersMap(..), arrayToMap, mapToArray)
import Sorting (SortOption, sortBySelectedOption)
import CSVParser (RankingCSV, parseRankingCSV)
import Data.Array (foldl) as Array
import Data.Int as DI
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Number as DN
import Data.String (trim)

fetchPlayers :: Aff (Either String (Map String Player))
fetchPlayers = do
  response <- AW.request $ defaultRequest
    { url = "./appData/rosters/activePlayers.json"
    , responseFormat = json
    }
  case response of
    Left err -> do
      let errorMsg = "Fetch Error: " <> AW.printError err
      pure $ Left errorMsg

    Right res -> do
      case decodeJson res.body of
        Right (ActivePlayers (PlayersMap playersMap)) -> do
          pure $ Right playersMap
        Left decodeError -> do
          let errorMsg = "Decode Error: " <> printJsonDecodeError decodeError
          pure $ Left errorMsg

mergeAndSortPlayers :: Map String Player -> RankingCSV -> SortOption -> Map String Player
mergeAndSortPlayers playersMap csvData defaultSort =
  let
    mergedPlayersMap = mergePlayerData playersMap csvData
    sortedPlayersArray = sortBySelectedOption defaultSort (mapToArray mergedPlayersMap)
  in
    arrayToMap sortedPlayersArray

mergePlayerData :: Map String Player -> RankingCSV -> Map String Player
mergePlayerData playersMap csvData = Array.foldl updatePlayerRanking playersMap csvData
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

fetchRankings :: Aff (Either String RankingCSV)
fetchRankings = do
  response <- AW.request $ defaultRequest 
    { url = "./appData/rosters/2023_Rankings.csv"
    , responseFormat = string 
    }
  case response of
    Left err -> pure $ Left $ AW.printError err
    Right res -> pure $ Right $ parseRankingCSV res.body