module Mutation
  ( RequestFunction
  , SortOption
  , SortValue(..)
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
import Data.Tuple (Tuple(..), snd)
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

-- filterActivePlayers :: forall a10 t12.
--   Eq a10 => Array a10
--             -> Array
--                  { primaryPosition :: a10
--                  | t12
--                  }
--                -> Array
--                     { primaryPosition :: a10
--                     | t12
--                     }
-- filterActivePlayers posCodes displayPlayers =
--   case posCodes of
--     [] -> displayPlayers
--     codes -> filter (\player -> elem player.primaryPosition codes) displayPlayers

filterActivePlayers :: Array String -> PlayersMap -> PlayersMap
filterActivePlayers posCodes (PlayersMap playersMap) =
  case posCodes of
    [] -> PlayersMap playersMap
    codes -> PlayersMap $ Map.filter (\player -> elem player.primaryPosition codes) playersMap

-- Sorting
type SortOption = String

sortOptions :: Array SortOption
sortOptions = ["ID", "Surname", "'23 Rank", "'23 Points"]

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

data SortValue
  = SortString String
  | SortNumber (Maybe Number)
  | SortInt (Maybe Int)

compareMaybes :: forall a. Ord a => Maybe a -> Maybe a -> Ordering
compareMaybes Nothing Nothing = EQ
compareMaybes Nothing (Just _) = GT
compareMaybes (Just _) Nothing = LT
compareMaybes (Just a) (Just b) = compare a b

instance eqSortValue :: Eq SortValue where
  eq x y = case (Tuple x y) of
    (Tuple (SortString a) (SortString b)) -> a == b
    (Tuple (SortNumber ma) (SortNumber mb)) -> ma == mb
    (Tuple (SortInt ma) (SortInt mb)) -> ma == mb
    _ -> false

instance ordSortValue :: Ord SortValue where
  compare x y = case (Tuple x y) of
    (Tuple (SortString a) (SortString b)) -> compare a b
    (Tuple (SortNumber ma) (SortNumber mb)) -> compareMaybes ma mb
    (Tuple (SortInt ma) (SortInt mb)) -> compareMaybes (map toNumber ma) (map toNumber mb)
    _ -> EQ
