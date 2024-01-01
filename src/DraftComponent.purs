module DraftComponent
  ( rankPlayersComponent
  )
  where

import Prelude

import Affjax.Web (request) as AW
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Data.Array (elem, intercalate, (:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy.NonEmpty as NE
import Data.List.NonEmpty (NonEmptyList, toList)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (Tuple(..), uncurry)
import Debug (class DebugWarning, spy, spyWith)
import Data.Map (Map)
import Data.Map as Map
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Effect.Console as CONSOLE
import Fetching (fetchPlayers, fetchRankings, mergeAndSortPlayers)
import Filtering (filterActivePlayers, toggleFilter)
import Halogen (ClassName(..), liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types.Player (Player, PlayersMap(..), RankingCSV, unwrapPlayersMap)
import Sorting (SortOption, sortBySelectedOption, sortOptions)
import Styles.Draft (cellStyle)
import Util.DraftUtils (getPositionDisplayValue, getTeamDisplayValue, position, showAsInt, spyShow)

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

data Query a = GetState (State -> a)

type State = {
    allPlayers :: PlayersMap
  , players :: PlayersMap
  , filterInputs :: Array String
  , currentSort :: SortOption
  , loading :: Boolean
  , error :: Maybe String
  , sortChangeFlag :: Boolean
}

initialState :: State
initialState = {
    allPlayers:  PlayersMap Map.empty
  , players:  PlayersMap Map.empty
  , filterInputs:  []
  , currentSort:  "ID"
  , loading:  true
  , error:  Nothing
  , sortChangeFlag:  false
}

data Action
  = TogglePositionFilter String
  | ResetFilters
  | ZeroFilters
  | SortBy SortOption
  | Initialize
  | HandleError String
  | DataFetched (Map String Player) RankingCSV
  
rankPlayersComponent :: forall q i m. MonadAff m => H.Component q i Void m
rankPlayersComponent = 
  H.mkComponent {
    initialState: \_ -> initialState
  , render
  , eval: H.mkEval $ H.defaultEval {
      handleAction = handleAction
    , initialize = Just Initialize
    }
  }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Void m Unit
handleAction = case _ of
  Initialize -> do
    H.liftEffect $ CONSOLE.log "Component initializing..."
    playerResult <- liftAff $ fetchPlayers AW.request
    case playerResult of
      Left err -> do
        H.liftEffect $ CONSOLE.log $ "Error fetching players: " <> err
        H.modify_ \s -> s { error = Just $ "Error fetching players: " <> err, loading = false }
      Right playersMap -> do
        rankingResult <- liftAff $ fetchRankings AW.request
        case rankingResult of
          Left err -> do
            H.liftEffect $ CONSOLE.log $ "Error fetching rankings: " <> err
            H.modify_ \s -> s { error = Just $ "Error fetching rankings: " <> err, loading = false }
          Right rankings -> do
            let defaultSort = "ID"
            let mergedAndSortedPlayers = mergeAndSortPlayers playersMap rankings defaultSort
            H.modify_ \s -> s { allPlayers = mergedAndSortedPlayers, players = mergedAndSortedPlayers, loading = false }
            H.liftEffect $ CONSOLE.log "Data successfully initialized, merged, and sorted"
            
  DataFetched playersMap rankings -> do
    oldState <- H.get
    let defaultSort = "ID"
    let mergedAndSortedPlayers = mergeAndSortPlayers (PlayersMap playersMap) rankings defaultSort
    let newState = updatePlayersView $ oldState { allPlayers = mergedAndSortedPlayers, players = mergedAndSortedPlayers }
    H.put newState

  ResetFilters -> do
    oldState <- H.get
    H.modify_ \s -> s { filterInputs = [], players = oldState.allPlayers }

  ZeroFilters -> do
    oldState <- H.get
    let newFilters = [""]
    let filteredPlayers = filterActivePlayers newFilters oldState.allPlayers
    let newState = oldState { filterInputs = newFilters, players = filteredPlayers }
    H.put newState

  TogglePositionFilter posCode -> do
    oldState <- H.get
    let newFilters = toggleFilter posCode oldState.filterInputs
    let filteredPlayers = filterActivePlayers newFilters oldState.allPlayers
    let newState = oldState { filterInputs = newFilters, players = filteredPlayers }
    H.put newState

  SortBy newSort -> do
    H.liftEffect $ CONSOLE.log "Sorting by: " <> logShow newSort
    oldState <- H.get
    -- _ <- H.liftEffect $ pure $ spyShow "Before Sorting: " oldState.players
    let newState = updatePlayersView $ oldState { currentSort = newSort, loading = true, sortChangeFlag = true }
    -- _ <- H.liftEffect $ pure $ spyShow "After Sorting: " newState.players
    _ <- H.liftEffect $ pure $ spy "Players Map: " newState
    H.put newState
    -- H.put $ spyHalogenState "After Sorting" newState  

  HandleError errorMsg -> 
    H.modify_ \s -> s { error = Just errorMsg, loading = false }

updatePlayersView :: State -> State
updatePlayersView currentState =
  let
    filteredPlayersMap = filterActivePlayers currentState.filterInputs currentState.players
    sortedFilteredPlayersMap = sortBySelectedOption currentState.currentSort filteredPlayersMap
  in
    currentState { players = sortedFilteredPlayersMap }

positionButtons :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
positionButtons state =
  HH.div_
    $ map createButton position
  where
    createButton :: (Tuple String String) -> H.ComponentHTML Action () m
    createButton (Tuple posName posCode) =
      HH.button
        [ HP.type_ ButtonButton
        , HE.onClick $ \_ -> TogglePositionFilter posCode
        , HP.classes $ if elem posCode state.filterInputs
                       then [ClassName "active"]
                       else []
        ]
        [ HH.text posName ]

resetButton :: forall m. MonadAff m => H.ComponentHTML Action () m
resetButton =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HE.onClick $ \_ -> ResetFilters
    , HP.classes [HH.ClassName "reset-button"]
    ]
    [ HH.text "Show All" ]

noneButton :: forall m. MonadAff m => H.ComponentHTML Action () m
noneButton =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HE.onClick $ \_ -> ZeroFilters
    , HP.classes [HH.ClassName "zero-button"]
    ]
    [ HH.text "Hide All" ]

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =   
  HH.div_
    [ positionButtons state
    , resetButton
    , noneButton 
    , sortDropdown state.currentSort
    , playersTable state.players  -- Pass PlayersMap directly
    ]

columnNames :: Array String
columnNames = ["ID ", "First ", "Last ", "Team ", "Pitch ", "Bat ", "Pos ", "Active ", "'23 Rank ", "'23 Points ", "NameSlug "]

renderPlayer :: forall m. MonadAff m => Tuple String Player -> H.ComponentHTML Action () m
renderPlayer (Tuple _ player) = 
  HH.tr_
    [ HH.td [ CSS.style cellStyle ] [ HH.text $ show player.playerId ]
    , HH.td [CSS.style cellStyle] [HH.text player.useName]
    , HH.td [ CSS.style cellStyle ] [ HH.text player.useLastName ]
    , HH.td [ CSS.style cellStyle ] [ HH.text $ getTeamDisplayValue player.currentTeam ]    
    , HH.td [ CSS.style cellStyle ] [ HH.text player.pitchHand ]
    , HH.td [ CSS.style cellStyle ] [ HH.text player.batSide ]   
    , HH.td [ CSS.style cellStyle ] [ HH.text $ getPositionDisplayValue player.primaryPosition ]
    , HH.td [ CSS.style cellStyle ] [ HH.text $ show player.active ]         
    , HH.td [ CSS.style cellStyle ] [ HH.text $ maybe "0" show player.past_ranking ]  
    , HH.td [ CSS.style cellStyle ] [ HH.text $ maybe "0" showAsInt player.past_fpts ]  
    , HH.td [ CSS.style cellStyle ] [ HH.text player.nameSlug ] 
    ]

playersTable :: forall m. MonadAff m => PlayersMap -> H.ComponentHTML Action () m
playersTable (PlayersMap players) = 
  HH.table_
    [ HH.thead_ 
        [ HH.tr_ $ map (\name -> HH.th_ [HH.text name]) columnNames ]
    , HH.tbody_ $ map renderPlayer $ Map.toUnfoldable players
    ]

sortDropdown :: forall m. MonadAff m => SortOption -> H.ComponentHTML Action () m
sortDropdown currentSort = 
  HH.select
    [ HE.onValueInput (SortBy) ]
    $ map (\option -> HH.option
                      [ HP.value option
                      , HP.selected $ option == currentSort
                      ] [ HH.text option ]) sortOptions