-- module TestModule where

module TestModule where


import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)


main :: Effect Unit
main = log "Hello, World!"

-- import Prelude

-- import Affjax.Node (defaultRequest)
-- import Affjax.Node as AN
-- import Affjax.ResponseFormat (json, string)
-- import CSVParser (RankingCSV)
-- import Data.Argonaut (jsonParser)
-- import Data.Array (take)
-- import Data.Array as Array
-- import Data.Either (Either(..))
-- import Data.List (all)
-- import Data.Map (Map)
-- import Data.Map as Map
-- import Data.Maybe (Maybe(..), maybe)
-- import Data.Unit (Unit)
-- import Effect (Effect)
-- import Effect.Aff (Aff, launchAff, launchAff_)
-- import Effect.Aff.Class (liftAff)
-- import Effect.Console as CONSOLE
-- import Fetching (RequestFunction, fetchPlayers, fetchRankings, mergeAndSortPlayers, mergePlayerData)
-- import Filtering (filterActivePlayers)
-- import Player (ActivePlayers(..), Player, PlayersMap(..), decodeJsonPlayerData, unwrapPlayersMap)
-- import Player (decodeJsonPlayerData, unwrapPlayersMap)
-- import Run (Run(..), liftEffect, runBaseAff', runBaseEffect, lift)
-- import Sorting (SortOption, sortBySelectedOption, sortOptions)

-- type TestState = {
--     allPlayers :: Maybe (Map String Player),
--     error :: Maybe String
-- }

-- type FilterCriteria = 
--   { position :: Maybe String
--   , team :: Maybe Int
--   , active :: Maybe Boolean
--   }

-- matchesCriteria :: FilterCriteria -> Player -> Boolean
-- matchesCriteria criteria player =
--   all (applyCriteria player) 
--     [ map (\p -> player.primaryPosition == p) criteria.position
--     , map (\t -> player.currentTeam == t) criteria.team
--     , map (\a -> player.active == a) criteria.active
--     ]
--   where
--     applyCriteria :: Player -> Maybe Boolean -> Boolean
--     applyCriteria _ Nothing = true
--     applyCriteria player (Just matches) = matches

-- initialTestState :: TestState
-- initialTestState = {
--     allPlayers: Nothing,
--     error: Nothing
-- }

-- processAndLogData :: forall eff. Map String Player -> Run (effect :: Effect | eff) Unit
-- processAndLogData mergedAndSortedPlayers =
--   liftEffect $ CONSOLE.log $ "Merged and Sorted Players: " <> show mergedAndSortedPlayers

-- processFetchedData :: PlayersMap -> RankingCSV -> SortOption -> FilterCriteria -> Map String Player
-- processFetchedData players rankings sortOption criteria =
--   let 
--     unwrappedPlayers = unwrapPlayersMap players
--     filteredPlayers = Map.filter (matchesCriteria criteria) unwrappedPlayers
--     mergedPlayersMap = mergePlayerData filteredPlayers rankings
--   in 
--     sortBySelectedOption sortOption mergedPlayersMap

-- testFetching :: forall eff. RequestFunction -> Run (effect :: Effect, aff :: Aff | eff) Unit
-- testFetching requestFunction = do
--   playerResponse <- liftEffect $ fetchPlayers (requestFunction (defaultRequest { responseFormat = json }))

--   case playerResponse of
--     Left err ->
--       lift $ CONSOLE.log $ "Error fetching players: " <> err

--     Right playerJson -> do
--       case decodeJsonPlayerData playerJson of
--         Left decodeErr ->
--           lift $ CONSOLE.log $ "Error decoding player data: " <> show decodeErr

--         Right playerData -> do
--           let playersMap = unwrapPlayersMap $ playerData.officialPlayers
--           rankingResult <- lift $ fetchRankings requestFunction (defaultRequest { responseFormat = string })
          
--           case rankingResult of
--             Left err ->
--               lift $ CONSOLE.log $ "Error fetching rankings: " <> err

--             Right rankings -> do
--                 let defaultSort = "ID"
--                 let filterCriteria = { position: Just "Pitcher", team: Nothing, active: Just true }
--                 let mergedAndSortedPlayers = processFetchedData playersMap rankings defaultSort filterCriteria
--                 lift $ processAndLogData mergedAndSortedPlayers

-- main :: Effect Unit
-- main = do
--   let requestFunction = AN.request -- or however you define it
--   launchAff_ $ runBaseAff' $ testFetching requestFunction