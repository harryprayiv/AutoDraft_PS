module DraftComponent
  ( rankPlayersComponent
  )
  where

import Prelude

import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.Web (post, printError, request) as AW
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console as CONSOLE
import Halogen (ClassName(..), liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Mutation (fetchPlayers, fetchRankings, filterActivePlayers, mergePlayerData, sortDisplayPlayers, toggleFilter)
import Styles.Draft (cellStyle)
import Types.Player (DisplayPlayer, DisplayPlayers, Player, PlayersMap(..), RankingCSV, SortOption, sortOptions, transformToDisplayPlayers, transformAndEncodeDisplayPlayers)
import Util.DraftUtils (getPositionDisplayValue, getTeamDisplayValue, position, showAsInt)

data Query a = GetState (State -> a)

type State = {
    allPlayers :: PlayersMap
  , displayPlayers :: DisplayPlayers
  , filterInputs :: Array String
  , currentSort :: SortOption
  , loading :: Boolean
  , error :: Maybe String
  , sortChangeFlag :: Boolean
  , sortOrder :: Boolean
}

initialState :: State
initialState = {
    allPlayers:  PlayersMap Map.empty
  , displayPlayers:  []
  , filterInputs:  []
  , currentSort:  "'23 Points"
  , loading:  true
  , error:  Nothing
  , sortChangeFlag:  false
  , sortOrder: false
}

data Action
  = TogglePositionFilter String
  | ResetFilters
  | ZeroFilters
  | SortBy SortOption
  | InvertSort
  | Initialize
  | SubmitRanking
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
            let defaultSort = "'23 Points"
            let newPlayersMap = mergePlayerData playersMap rankings
            let newDisplayPlayers = sortDisplayPlayers defaultSort false $ transformToDisplayPlayers newPlayersMap
            H.modify_ \s -> s { allPlayers = newPlayersMap, displayPlayers = newDisplayPlayers, loading = false }
            H.liftEffect $ CONSOLE.log "Data successfully initialized, merged, and sorted"

  SubmitRanking -> do
      state <- H.get
      let jsonBody = transformAndEncodeDisplayPlayers state.displayPlayers
      let requestBody = RB.Json jsonBody  -- Prepare the JSON payload
      -- Make the POST request specifying RF.json for the response format
      response <- liftAff $ AW.post RF.json "/submit-ranking" (Just requestBody)
      case response of
        Left err -> do
          -- Use printError to convert the Error to a String
          H.liftEffect $ CONSOLE.log $ "Error submitting ranking: " <> AW.printError err
        Right _ -> do
          H.liftEffect $ CONSOLE.log "Ranking submitted successfully"
      pure unit

  DataFetched playersMap rankings -> do
      oldState <- H.get
      let defaultSort = "'23 Points"
      let newPlayersMap = mergePlayerData (PlayersMap playersMap) rankings
      let newDisplayPlayers = sortDisplayPlayers defaultSort false $ transformToDisplayPlayers newPlayersMap
      H.put $ oldState { allPlayers = newPlayersMap, displayPlayers = newDisplayPlayers }

  ResetFilters -> do
    oldState <- H.get
    H.modify_ \s -> s { filterInputs = [], displayPlayers = transformToDisplayPlayers oldState.allPlayers }

  ZeroFilters -> do
    oldState <- H.get
    let newFilters = [""]
    let filteredPlayersMap = filterActivePlayers newFilters oldState.allPlayers
    let filteredDisplayPlayers = transformToDisplayPlayers filteredPlayersMap
    H.modify_ \s -> s { filterInputs = newFilters, displayPlayers = filteredDisplayPlayers }

  TogglePositionFilter posCode -> do
    oldState <- H.get
    let newFilters = toggleFilter posCode oldState.filterInputs
    let filteredPlayersMap = filterActivePlayers newFilters oldState.allPlayers
    let filteredDisplayPlayers = transformToDisplayPlayers filteredPlayersMap
    H.modify_ \s -> s { filterInputs = newFilters, displayPlayers = filteredDisplayPlayers }

  SortBy newSort -> do
    oldState <- H.get
    let sortedDisplayPlayers = sortDisplayPlayers newSort oldState.sortOrder oldState.displayPlayers
    H.modify_ \s -> s { currentSort = newSort, displayPlayers = sortedDisplayPlayers }

  InvertSort -> do
    oldState <- H.get
    let newSortOrder = not oldState.sortOrder
    let reSortedDisplayPlayers = sortDisplayPlayers oldState.currentSort newSortOrder oldState.displayPlayers
    H.modify_ \s -> s { sortOrder = newSortOrder, displayPlayers = reSortedDisplayPlayers }

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

submitButton :: forall m. MonadAff m => H.ComponentHTML Action () m
submitButton =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HE.onClick $ \_ -> SubmitRanking
    , HP.classes [HH.ClassName "submit-button"]
    ]
    [ HH.text "Submit" ]    

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
    , submitButton
    , playersTable state.displayPlayers
    ]

columnNames :: Array String
columnNames = ["ID ", "First ", "Last ", "Team ", "Pitch ", "Bat ", "Pos ", "Active ", "'23 Rank ", "'23 Points ", "NameSlug "]

renderPlayer :: forall m. MonadAff m => DisplayPlayer -> H.ComponentHTML Action () m
renderPlayer player = 
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

playersTable :: forall m. MonadAff m => DisplayPlayers -> H.ComponentHTML Action () m
playersTable players =
  HH.table_
    [ HH.thead_ [ HH.tr_ $ map (\name -> HH.th_ [ HH.text name ]) columnNames ]
    , HH.tbody_ $ map renderPlayer players
    ]

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

-- downloadJson :: forall m. MonadAff m => String -> String -> m Unit
-- downloadJson fileName jsonStr = liftAff do
--   win <- window
--   doc <- document win
--   blob <- newBlob [jsonStr] (BlobPropertyBag { type: "application/json" })
--   url <- createObjectURL blob
--   anchor <- createElement doc "a"
--   case anchor of
--     Just elem -> do
--       let a = unsafeCoerce elem :: HTMLAnchorElement
--       set href url a
--       set download fileName a
--       click a
--       pure unit
--     Nothing -> pure unit