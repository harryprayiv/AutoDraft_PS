module Fetching
  ( RequestFunction
  , fetchPlayers
  , fetchRankings
  , mergeAndSortPlayers
  , mergePlayerData
  )
  where

import Prelude

import Affjax (Error, Request, Response, defaultRequest, printError)
import Affjax.ResponseFormat (json, string)
import CSVParser (RankingCSV, parseRankingCSV)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (foldl) as Array
import Data.Either (Either(..))
import Data.Int as DI
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Number as DN
import Data.String (trim)
import Effect.Aff (Aff)
import Player (ActivePlayers(..), Player, PlayersMap(..))
import Sorting (SortOption, sortBySelectedOption)

type RequestFunction = forall a. Request a -> Aff (Either Error (Response a))

fetchPlayers :: RequestFunction -> Aff (Either String (Map String Player))
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
        Right (ActivePlayers (PlayersMap playersMap)) -> do
          pure $ Right playersMap
        Left decodeError -> do
          let errorMsg = "Decode Error: " <> printJsonDecodeError decodeError
          pure $ Left errorMsg

fetchRankings :: RequestFunction -> Aff (Either String RankingCSV)
fetchRankings requestFunction = do
  response <- requestFunction $ defaultRequest 
    { url = "./testData/fake_Rankings.csv"
    , responseFormat = string 
    }
  case response of
    Left err -> pure $ Left $ printError err
    Right res -> pure $ Right $ parseRankingCSV res.body

mergeAndSortPlayers :: Map String Player -> RankingCSV -> SortOption -> Map String Player
mergeAndSortPlayers playersMap csvData defaultSort =
  let
    mergedPlayersMap = mergePlayerData playersMap csvData
    sortedPlayersMap = sortBySelectedOption defaultSort mergedPlayersMap
  in
    sortedPlayersMap

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
