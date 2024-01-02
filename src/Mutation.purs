module Mutation
  ( RequestFunction
  , SortOption
  , compareMaybes
  , fetchPlayers
  , fetchRankings
  , filterActivePlayers
  , fromSortableList
  , mergeAndSortPlayers
  , mergePlayerData
  , sortBySelectedOption
  , sortDisplayPlayers
  , sortDisplayPlayersBy
  , sortOptions
  , sortPlayersBy
  , toSortableList
  , toggleFilter
  )
  where

import Prelude

import Affjax (Error, Request, Response, defaultRequest, printError)
import Affjax.ResponseFormat (json, string)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (elem, filter, (\\))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as DI
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Number as DN
import Data.String (trim)
import Data.Tuple (Tuple, snd)
import Effect.Aff (Aff)
import Types.Player (ActivePlayers(..), DisplayPlayer, DisplayPlayers, Player, PlayersMap(..), RankingCSV, parseRankingCSV, transformToDisplayPlayers)

-- Fetching
type RequestFunction = forall a. Request a -> Aff (Either Error (Response a))

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
    { url = "./testData/fake_Rankings.csv"
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
    sortedDisplayPlayers = sortDisplayPlayers sortOption displayPlayers
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

filterActivePlayers :: forall a10 t12.
  Eq a10 => Array a10
            -> Array
                 { primaryPosition :: a10
                 | t12
                 }
               -> Array
                    { primaryPosition :: a10
                    | t12
                    }
filterActivePlayers posCodes displayPlayers =
  case posCodes of
    [] -> displayPlayers
    codes -> filter (\player -> elem player.primaryPosition codes) displayPlayers


-- Sorting
type SortOption = String

sortOptions :: Array SortOption
sortOptions = ["ID", "Surname", "'23 Rank", "'23 Points"]

toSortableList :: PlayersMap -> List (Tuple String Player)
toSortableList (PlayersMap playersMap) = Map.toUnfoldable playersMap

fromSortableList :: List (Tuple String Player) -> PlayersMap
fromSortableList = PlayersMap <<< Map.fromFoldable

sortPlayersBy :: forall a. Ord a => (Player -> Maybe a) -> List (Tuple String Player) -> List (Tuple String Player)
sortPlayersBy f playersList =
  List.sortBy (\x y -> compareMaybes (f (snd x)) (f (snd y))) playersList

compareMaybes :: forall a. Ord a => Maybe a -> Maybe a -> Ordering
compareMaybes Nothing Nothing = EQ
compareMaybes Nothing (Just _) = LT
compareMaybes (Just _) Nothing = GT
compareMaybes (Just a) (Just b) = compare a b

sortDisplayPlayersBy :: forall a. Ord a => (DisplayPlayer -> Maybe a) -> DisplayPlayers -> DisplayPlayers
sortDisplayPlayersBy f playersArray =
  Array.sortBy (\x y -> compareMaybes (f x) (f y)) playersArray

sortDisplayPlayers :: SortOption -> DisplayPlayers -> DisplayPlayers
sortDisplayPlayers sortOption players =
  case sortOption of
    "ID" -> sortDisplayPlayersBy (Just <<< _.id) players
    "Surname" -> sortDisplayPlayersBy (Just <<< _.useLastName) players
    "'23 Rank" -> sortDisplayPlayersBy _.past_ranking players
    "'23 Points" -> sortDisplayPlayersBy _.past_fpts players
    _ -> players -- Default case, could be unsorted or sorted by a default field

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