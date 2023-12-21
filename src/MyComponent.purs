module MyComponent
  ( component
  )
  where

import Prelude

import CSS.Border as Border
import CSS.Color as Color
import CSS.Geometry (paddingBottom, paddingLeft, paddingRight, paddingTop) as CSS
import CSS.Size as Size
import CSS.Stylesheet (CSS)
import CSVParser (RankingCSV)
import Data.Array (find)
import Data.Either (Either(..))
import Data.Int (trunc)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as DEBUG
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Player (Player, position, teams)
import Sorting (SortOption, fetchPlayers, fetchRankings, filterActivePlayers, mergeAndSortPlayers, sortBySelectedOption, sortOptions)

data Query a = GetState (State -> a)

type State = {
    allPlayers :: Map String Player
  , players :: Map String Player
  , filterInput :: String
  , currentSort :: SortOption
  , loading :: Boolean
  , error :: Maybe String
  , sortChangeFlag :: Boolean
}

initialState :: State
initialState = {
    allPlayers: Map.empty
  , players: Map.empty
  , filterInput: ""
  , currentSort: "'23 Rank"  -- Set a default sort option
  , loading: true
  , error: Nothing
  , sortChangeFlag: false
}

data Action
  = FilterBy String
  | SortBy SortOption
  | Initialize
  | HandleError String
  | DataFetched (Map String Player) RankingCSV
  
component :: forall q i m. MonadAff m => H.Component q i Void m
component = 
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
    H.liftEffect $ DEBUG.log "Component initializing..."
    playerResult <- liftAff fetchPlayers
    case playerResult of
      Left err -> do
        H.liftEffect $ DEBUG.log $ "Error fetching players: " <> err
        H.modify_ \s -> s { error = Just $ "Error fetching players: " <> err, loading = false }
      Right playersMap -> do
        rankingResult <- liftAff fetchRankings
        case rankingResult of
          Left err -> do
            H.liftEffect $ DEBUG.log $ "Error fetching rankings: " <> err
            H.modify_ \s -> s { error = Just $ "Error fetching rankings: " <> err, loading = false }
          Right rankings -> do
            let defaultSort = "'23 Rank"
            let mergedAndSortedPlayers = mergeAndSortPlayers playersMap rankings defaultSort
            H.modify_ \s -> s { allPlayers = mergedAndSortedPlayers, players = mergedAndSortedPlayers, loading = false }
            H.liftEffect $ DEBUG.log "Data successfully initialized, merged, and sorted"

  DataFetched playersMap rankings -> do
    let mergedAndSortedPlayers = mergeAndSortPlayers playersMap rankings initialState.currentSort
    H.modify_ \s -> s { allPlayers = mergedAndSortedPlayers, players = mergedAndSortedPlayers, loading = false }
    updatePlayersView

  SortBy newSort -> do
    H.liftEffect $ DEBUG.log $ "Sorting by: " <> newSort
    H.modify_ \s -> s { currentSort = newSort }
    updatePlayersView

  FilterBy position -> do
    H.liftEffect $ DEBUG.log $ "Filtering by position: " <> position
    H.modify_ \s -> s { filterInput = position }
    updatePlayersView

  HandleError errorMsg -> 
    H.modify_ \s -> s { error = Just errorMsg, loading = false }

updatePlayersView :: forall m. H.HalogenM State Action () Void m Unit
updatePlayersView = do
  currentState <- H.get
  let filteredPlayers = filterActivePlayers currentState.filterInput currentState.allPlayers
  let sortedFilteredPlayers = sortBySelectedOption currentState.currentSort filteredPlayers
  H.modify_ \s -> s { players = sortedFilteredPlayers }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =   
  HH.div_
    [ filterDropdown state
    , sortDropdown state.currentSort
    , playersTable state.players
    ]

columnNames :: Array String
columnNames = ["MLB_ID", "First", "Last", "Team", "Pitch", "Bat", "Pos", "Active", "'23 Rank", "'23 Points", "NameSlug"]

cellStyle :: CSS
cellStyle = do
  Border.border Border.solid (Size.px 1.0) Color.black
  CSS.paddingTop (Size.px 0.4)
  CSS.paddingBottom (Size.px 0.4)
  CSS.paddingLeft (Size.px 5.0)
  CSS.paddingRight (Size.px 5.0)

renderPlayer :: forall m. MonadAff m => Tuple String Player -> H.ComponentHTML Action () m
renderPlayer (Tuple _ player) = 
  HH.tr_
    [ HH.td [ CSS.style cellStyle ] [ HH.text $ show player.playerId ]
    , HH.td [CSS.style cellStyle] [HH.text player.useName]
    , HH.td [ CSS.style cellStyle ] [ HH.text player.useLastName ]
    , HH.td [ CSS.style cellStyle ] [ HH.text $ getTeamDisplayValue player.currentTeam ]    
    , HH.td [ CSS.style cellStyle ] [ HH.text player.pitchHand ]
    , HH.td [ CSS.style cellStyle ] [ HH.text player.batSide ]   
    , HH.td [ CSS.style cellStyle ] [ HH.text $ getDisplayValue player.primaryPosition ]
    , HH.td [ CSS.style cellStyle ] [ HH.text $ show player.active ]         
    , HH.td [ CSS.style cellStyle ] [ HH.text $ maybe "N/A" show player.past_ranking ]  
    , HH.td [ CSS.style cellStyle ] [ HH.text $ maybe "0" showAsInt player.past_fpts ]  
    , HH.td [ CSS.style cellStyle ] [ HH.text player.nameSlug ] 
    ]

playersTable :: forall m. MonadAff m => Map String Player -> H.ComponentHTML Action () m
playersTable players = 
  HH.table_
    [ HH.thead_ 
        [ HH.tr_ $ map (\name -> HH.th_ [HH.text name]) columnNames ]
    , HH.tbody_ $ map renderPlayer $ Map.toUnfoldable players
    ]

getDisplayValue :: String -> String
getDisplayValue value =
  maybe value fst (find (\(Tuple _ code) -> code == value) position)

getTeamDisplayValue :: Int -> String
getTeamDisplayValue value =
  maybe (show value) fst (find (\(Tuple _ code) -> code == value) teams)

showAsInt :: Number -> String
showAsInt num = show $ trunc num

filterDropdown :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
filterDropdown state = 
  HH.select
    [ HE.onValueInput (FilterBy) ]
    $ map (\(Tuple displayName filterString) -> HH.option
           [ HP.value filterString
           , HP.selected $ filterString == state.filterInput
           ] [ HH.text displayName ]) position

sortDropdown :: forall m. MonadAff m => SortOption -> H.ComponentHTML Action () m
sortDropdown currentSort = 
  HH.select
    [ HE.onValueInput (SortBy) ]
    $ map (\option -> HH.option
                      [ HP.value option
                      , HP.selected $ option == currentSort
                      ] [ HH.text option ]) sortOptions
