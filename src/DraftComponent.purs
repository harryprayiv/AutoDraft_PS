module DraftComponent
  ( rankPlayersComponent
  )
  where

import Prelude

import Affjax.Web (request) as AW
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Data.Array (deleteAt, elem, index, mapWithIndex, splitAt)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console as CONSOLE
import Halogen (ClassName(..), liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Styles.Draft (cellStyle)
import Mutation (fetchPlayers, fetchRankings, filterActivePlayers, mergeAndSortPlayers, sortDisplayPlayers, sortOptions, toggleFilter) 
import Types.Player (DisplayPlayer, DisplayPlayers, Player, PlayersMap(..), RankingCSV, SortOption, State) 
import Util.DraftUtils (getPositionDisplayValue, getTeamDisplayValue, position, showAsInt)

initialState :: State
initialState = {
    allPlayers:  PlayersMap Map.empty
  , displayPlayers:  []
  , filterInputs:  []
  , currentSort:  "ID"
  , loading:  true
  , error:  Nothing
  , sortChangeFlag:  false
  , sortOrder: false
  , dragIndex: Nothing
  , dropIndex: Nothing
  , manualOrdering: false 
}

data Action
  = TogglePositionFilter String
  | ResetFilters
  | ZeroFilters
  | SortBy SortOption
  | InvertSort
  | Initialize
  | HandleError String
  | DataFetched (Map String Player) RankingCSV
  | StartDrag Int
  | EndDrag
  | DropPlayer Int
  
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
            let newState = mergeAndSortPlayers (initialState { allPlayers = playersMap }) rankings "'23 Points"
            H.put newState
            H.liftEffect $ CONSOLE.log "Data successfully initialized, merged, and sorted"

  DataFetched _ rankings -> do
      oldState <- H.get
      let newState = mergeAndSortPlayers oldState rankings "'23 Points"
      H.put newState

  ResetFilters -> do
    oldState <- H.get
    let newState = filterActivePlayers [] oldState{ filterInputs = [], manualOrdering = false }
    H.put newState

  ZeroFilters -> do
    oldState <- H.get
    let newState = filterActivePlayers [""] oldState{ filterInputs = [""], manualOrdering = false }
    H.put newState

  TogglePositionFilter posCode -> do
    oldState <- H.get
    let newFilters = toggleFilter posCode oldState.filterInputs
    let newState = filterActivePlayers newFilters oldState{ filterInputs = newFilters }
    H.put newState

  SortBy newSort -> do
    oldState <- H.get
    let newState = sortDisplayPlayers newSort oldState.sortOrder oldState
    H.put newState

  InvertSort -> do
    oldState <- H.get
    let newState = sortDisplayPlayers oldState.currentSort (not oldState.sortOrder) oldState
    H.put newState

  StartDrag index -> H.modify_ \s -> s { dragIndex = Just index, manualOrdering = true }
  EndDrag -> H.modify_ \s -> s { dragIndex = Nothing, dropIndex = Nothing, manualOrdering = false }
  DropPlayer index -> do
    oldState <- H.get
    case oldState.dragIndex of
      Just dragIdx -> do
        let updatedPlayers = movePlayer dragIdx index oldState.displayPlayers
        H.modify_ \s -> s { displayPlayers = updatedPlayers, dragIndex = Nothing, dropIndex = Nothing, manualOrdering = false }
      Nothing -> pure unit

  HandleError errorMsg -> 
    H.modify_ \s -> s { error = Just errorMsg, loading = false }

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
    , invertButton
    , playersTable state.displayPlayers
    ]

columnNames :: Array String
columnNames = ["ID ", "First ", "Last ", "Team ", "Pitch ", "Bat ", "Pos ", "Active ", "'23 Rank ", "'23 Points ", "NameSlug "]

renderPlayer :: forall m. MonadAff m => Int -> DisplayPlayer -> H.ComponentHTML Action () m
renderPlayer index player = 
  HH.tr
    [ HE.onDragStart $ \_ -> StartDrag index
    , HE.onDragOver $ \_ -> DropPlayer index
    , HE.onDragEnd $ \_ -> EndDrag
    , HP.draggable true
    ]
    [
      HH.td [ CSS.style cellStyle ] [ HH.text $ show player.playerId ]
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

playersTable :: forall m. MonadAff m => DisplayPlayers -> H.ComponentHTML Action () m
playersTable players =
  HH.table_
    [ HH.thead_ [ HH.tr_ $ map (\name -> HH.th_ [ HH.text name ]) columnNames ]
    , HH.tbody_ $ mapWithIndex renderPlayer players
    ]

movePlayer :: Int -> Int -> DisplayPlayers -> DisplayPlayers
movePlayer from to players =
  let { before, after } = splitAt to players
      movedPlayer = index players from
  in case movedPlayer of
       Just player ->
         let adjustedAfter = fromMaybe after $ deleteAt (from - if from < to then 0 else to) after
         in before <> [player] <> adjustedAfter
       Nothing -> players

sortDropdown :: forall m. MonadAff m => SortOption -> H.ComponentHTML Action () m
sortDropdown currentSort = 
  HH.select
    [ HE.onValueInput (SortBy) ]
    $ map (\option -> HH.option
                      [ HP.value option
                      , HP.selected $ option == currentSort
                      ] [ HH.text option ]) sortOptions

invertButton :: forall m. MonadAff m => H.ComponentHTML Action () m
invertButton =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HE.onClick $ \_ -> InvertSort
    , HP.classes [HH.ClassName "invert-button"]
    ]
    [ HH.text "invert" ]