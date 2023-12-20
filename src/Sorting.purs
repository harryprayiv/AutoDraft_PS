module Sorting where

import Prelude
import Affjax (defaultRequest)
import Affjax.ResponseFormat (json, string)
import Affjax.Web as AW
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (find, sortBy)
import Data.Array (foldl) as Array
import Data.Either (Either(..))
import Data.Int (trunc)
import Data.Int as DI
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Number as DN
import Data.String (trim)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Player (ActivePlayers(..), Player, PlayersMap(..), position, teams) 
import CSVParser (RankingCSV, parseRankingCSV)

getDisplayValue :: String -> String
getDisplayValue value =
  maybe value fst (find (\(Tuple _ code) -> code == value) position)

getTeamDisplayValue :: Int -> String
getTeamDisplayValue value =
  maybe (show value) fst (find (\(Tuple _ code) -> code == value) teams)

showAsInt :: Number -> String
showAsInt num = show $ trunc num

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

filterActivePlayers :: String -> Map String Player -> Map String Player
filterActivePlayers filterInput playersMap =
  if filterInput == "" then playersMap
  else Map.filter (\player -> player.primaryPosition == filterInput) playersMap

fetchRankings :: Aff (Either String RankingCSV)
fetchRankings = do
  response <- AW.request $ defaultRequest 
    { url = "./appData/rosters/2023_Rankings.csv"
    , responseFormat = string 
    }
  case response of
    Left err -> pure $ Left $ AW.printError err
    Right res -> pure $ Right $ parseRankingCSV res.body

mergeAndSortPlayers :: Map String Player -> RankingCSV -> SortOption -> Map String Player
mergeAndSortPlayers playersMap csvData defaultSort = 
  let
    mergedPlayers = mergePlayerData playersMap csvData
  in
    sortBySelectedOption defaultSort mergedPlayers

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

type SortOption = String

sortOptions :: Array SortOption
sortOptions = ["MLB ID (default)", "Surname", "'23 Rank", "'23 Points"]

sortBySelectedOption :: SortOption -> Map String Player -> Map String Player
sortBySelectedOption sortOption playersMap = 
  case sortOption of
    "MLB ID (default)" -> sortByMLBid playersMap
    "Surname" -> sortBySurname playersMap
    "'23 Rank" -> sortByRank playersMap
    "'23 Points" -> sortByPoints playersMap
    _ -> playersMap

sortByMLBid :: Map String Player -> Map String Player
sortByMLBid = sortPlayersBy (\player -> Just player.playerId)

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
