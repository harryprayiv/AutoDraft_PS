module TestModule where

-- import Prelude
-- import Sorting
-- import Effect.Aff (Aff)
-- import Data.Tuple (Tuple(..))
-- import Effect.Class.Console (logShow)
-- import Data.Either (Either(..))

-- -- Function to run the test
-- testSorting :: Aff Unit
-- testSorting = do
--   -- eitherPlayersMap <- fetchPlayers
--   -- eitherRankings <- fetchRankings

--   -- case (Tuple (eitherPlayersMap) (eitherRankings)) of
--   --   Tuple (Right playersMap) (Right rankings) -> do
--   --     let defaultSort = "Surname"
--   --     let mergedAndSortedPlayers = mergeAndSortPlayers playersMap rankings defaultSort
--   --     logShow mergedAndSortedPlayers

--   --   Tuple (Left err) (_) -> logShow $ "Error fetching players: " <> err
--   --   Tuple (_) (Left err) -> logShow $ "Error fetching rankings: " <> err
