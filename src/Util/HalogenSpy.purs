module Util.HalogenSpy where




-- showPlayersMap :: PlayersMap -> String
-- showPlayersMap (PlayersMap playersMap) =
--   let
--     showPlayer :: String -> Player -> String
--     showPlayer key player = "{ Key: " <> key <> ", Player: " <> show player <> " }"

--     playersList :: List (Tuple String Player)
--     playersList = Map.toUnfoldable playersMap

--     maybeNonEmptyPlayersList :: Maybe (NonEmptyList (Tuple String Player))
--     maybeNonEmptyPlayersList = toNonEmptyArray playersList

--     showNonEmptyPlayers :: Maybe (NonEmptyList (Tuple String Player)) -> String
--     showNonEmptyPlayers Nothing = "Empty"
--     showNonEmptyPlayers (Just nel) = intercalate ", " (map (uncurry showPlayersMap) (toListNEL nel))
--   in "[" <> showNonEmptyPlayers maybeNonEmptyPlayersList <> "]"

-- toListNEL :: forall a. NonEmptyList a -> List a
-- toListNEL nel = toList nel

-- showNonEmptyPlayers :: Maybe (NonEmptyList (Tuple String Player)) -> String
-- showNonEmptyPlayers Nothing = "Empty"
-- showNonEmptyPlayers (Just nel) = intercalate ", " (map (uncurry showPlayersMap) (toListNEL nel))

-- toNonEmptyArray :: forall a. Array a -> Maybe (NonEmpty Array a)
-- toNonEmptyArray arr = case Array.uncons arr of
--   Nothing -> Nothing
--   Just { head: x, tail: xs } -> Just (x :| xs)

-- spyHalogenState :: DebugWarning => String -> State -> State
-- spyHalogenState msg state = 
--   let
--     stateString = "{ allPlayers: " <> showPlayersMap state.allPlayers
--                    <> ", players: " <> showPlayersMap state.players
--                    <> ", filterInputs: " <> show state.filterInputs
--                    <> ", currentSort: " <> show state.currentSort
--                    <> ", loading: " <> show state.loading
--                    <> ", error: " <> show state.error
--                    <> ", sortChangeFlag: " <> show state.sortChangeFlag
--                    <> " }"
--   in
--     spyWith msg (\_ -> stateString) state                      