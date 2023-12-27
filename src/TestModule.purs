module TestModule where

-- import Prelude

-- import Affjax.Node as AN
-- import CSVParser (RankingCSV)
-- import Control.Monad.ST as Array
-- import Data.Argonaut (jsonParser)
-- import Data.Array (take)
-- import Data.Array as Array
-- import Data.Either (Either(..))
-- import Data.Map (Map)
-- import Data.Map as Map
-- import Data.Maybe (Maybe(..), maybe)
-- import Effect (Effect)
-- import Effect.Aff.Class (liftAff)
-- import Effect.Console as CONSOLE
-- import Fetching (fetchPlayers, fetchRankings, mergeAndSortPlayers)
-- import Filtering (filterActivePlayers)
-- import Player (ActivePlayers(..), Player, PlayersMap(..), arrayToMap, decodeJsonPlayerData, mapToArray, unwrapPlayersMap)
-- import Sorting (SortOption, sortBySelectedOption, sortOptions)

-- type TestState = {
--     allPlayers :: Maybe (Map String Player),
--     error :: Maybe String
-- }

-- initialTestState :: TestState
-- initialTestState = {
--     allPlayers: Nothing,
--     error: Nothing
-- }

-- testMockPlayers :: Effect Unit
-- testMockPlayers = do
--   result <- mockPlayers json
--   case result of
--     Right activePlayers -> CONSOLE.log $ "Active Players decoded successfully: " <> show activePlayers
--     Left errorMsg -> CONSOLE.log $ "Error: " <> errorMsg

-- testSorting :: Effect Unit
-- testSorting = do
--     CONSOLE.log "Testing Sorting..."
--     let playersArray = Map.toUnfoldable (unwrapPlayersMap mockPlayers)
--     Array.foreach sortOptions \sortOption -> do
--         let sortedPlayers = sortBySelectedOption sortOption playersArray
--         CONSOLE.log $ "Sorted by " <> sortOption <> ":"
--         CONSOLE.logShow $ take 5 sortedPlayers -- Show first 5 players for brevity

-- testFiltering :: Effect Unit
-- testFiltering = do
--     CONSOLE.log "Testing Filtering..."
--     let positions = ["1", "3"] -- Example positions to filter
--     let filteredPlayers = filterActivePlayers positions (unwrapPlayersMap mockPlayers)
--     CONSOLE.log $ "Filtered players for positions " <> show positions <> ":"
--     CONSOLE.logShow $ take 5 (Map.toUnfoldable filteredPlayers) -- Show first 5 players for brevity


-- main :: Effect Unit
-- main = do
--     testSorting
--     testFiltering
 
-- csv :: String
-- csv = """
-- MLB,CBS,Name,FPTS_2023,rank_2023
-- 660670,2211777,Ronald Acuna,817,1
-- 518692,1630079,Freddie Freeman,666.5,2
-- 621566,2044509,Matt Olson,659.5,3
-- 605141,2106654,Mookie Betts,646.5,4
-- 543037,1893753,Gerrit Cole,622,5
-- 663656,2184352,Kyle Tucker,598,6
-- 665742,2507367,Juan Soto,589.5,7
-- 543760,1947827,Marcus Semien,589,8
-- 682998,3117913,Corbin Carroll,588.5,9
-- 660271,2901324,Shohei Ohtani,578.5,10
-- 675911,2837573,Spencer Strider,576.5,11
-- 677951,3117472,Bobby Witt,576.5,12
-- 677594,2825528,Julio Rodriguez,556.5,13
-- 596019,1894627,Francisco Lindor,556.5,14
-- 668678,2914671,Zac Gallen,553,15
-- """

-- json :: String
-- json = """
-- {"checksum":"795fa1d16a8896ce48f643032d0de5c65817cc797f74cc345db941e1e3900511","dataPulled":"2023_12_12_14_02","officialPlayers":{"660670":{"active":true,"batSide":"R","currentTeam":144,"nameSlug":"ronald-acuna-jr-660670","pitchHand":"R","playerId":660670,"primaryPosition":"9","useLastName":"Acuña Jr.","useName":"Ronald"},
-- "518692":{"active":true,"batSide":"L","currentTeam":119,"nameSlug":"freddie-freeman-518692","pitchHand":"R","playerId":518692,"primaryPosition":"3","useLastName":"Freeman","useName":"Freddie"},
-- "621566":{"active":true,"batSide":"L","currentTeam":144,"nameSlug":"matt-olson-621566","pitchHand":"R","playerId":621566,"primaryPosition":"3","useLastName":"Olson","useName":"Matt"},
-- "605141":{"active":true,"batSide":"R","currentTeam":119,"nameSlug":"mookie-betts-605141","pitchHand":"R","playerId":605141,"primaryPosition":"9","useLastName":"Betts","useName":"Mookie"},
-- "543037":{"active":true,"batSide":"R","currentTeam":147,"nameSlug":"gerrit-cole-543037","pitchHand":"R","playerId":543037,"primaryPosition":"1","useLastName":"Cole","useName":"Gerrit"},
-- "663656":{"active":true,"batSide":"L","currentTeam":117,"nameSlug":"kyle-tucker-663656","pitchHand":"R","playerId":663656,"primaryPosition":"9","useLastName":"Tucker","useName":"Kyle"},
-- "665742":{"active":true,"batSide":"L","currentTeam":147,"nameSlug":"juan-soto-665742","pitchHand":"L","playerId":665742,"primaryPosition":"7","useLastName":"Soto","useName":"Juan"},
-- "543760":{"active":true,"batSide":"R","currentTeam":140,"nameSlug":"marcus-semien-543760","pitchHand":"R","playerId":543760,"primaryPosition":"4","useLastName":"Semien","useName":"Marcus"},
-- "682998":{"active":true,"batSide":"L","currentTeam":109,"nameSlug":"corbin-carroll-682998","pitchHand":"L","playerId":682998,"primaryPosition":"7","useLastName":"Carroll","useName":"Corbin"},
-- "660271":{"active":true,"batSide":"L","currentTeam":119,"nameSlug":"shohei-ohtani-660271","pitchHand":"R","playerId":660271,"primaryPosition":"Y","useLastName":"Ohtani","useName":"Shohei"},
-- "675911":{"active":true,"batSide":"R","currentTeam":144,"nameSlug":"spencer-strider-675911","pitchHand":"R","playerId":675911,"primaryPosition":"1","useLastName":"Strider","useName":"Spencer"},
-- "677951":{"active":true,"batSide":"R","currentTeam":118,"nameSlug":"bobby-witt-jr-677951","pitchHand":"R","playerId":677951,"primaryPosition":"6","useLastName":"Witt Jr.","useName":"Bobby"},
-- "677594":{"active":true,"batSide":"R","currentTeam":136,"nameSlug":"julio-rodriguez-677594","pitchHand":"R","playerId":677594,"primaryPosition":"8","useLastName":"Rodríguez","useName":"Julio"},
-- "596019":{"active":true,"batSide":"S","currentTeam":121,"nameSlug":"francisco-lindor-596019","pitchHand":"R","playerId":596019,"primaryPosition":"6","useLastName":"Lindor","useName":"Francisco"},
-- "668678":{"active":true,"batSide":"R","currentTeam":109,"nameSlug":"zac-gallen-668678","pitchHand":"R","playerId":668678,"primaryPosition":"1","useLastName":"Gallen","useName":"Zac"}}}
-- """

-- mockPlayers :: String -> Effect (Either String ActivePlayers)
-- mockPlayers jsonString = do
--   case jsonParser jsonString of
--     Right jsonVal -> pure $ case decodeJsonPlayerData jsonVal of
--       Right players -> Right players
--       Left decodeError -> Left $ "Decoding Error: " <> show decodeError
--     Left parseError -> pure $ Left $ "Parsing Error: " <> show parseError