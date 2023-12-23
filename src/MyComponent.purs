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
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Data.Array (elem, filter, find, (:), (\\))
import Data.Either (Either(..), either)
import Data.Int (trunc)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst)
import Debug (spy)
import Affjax (defaultRequest)
import Affjax.ResponseFormat (json, string)
import Affjax.Web as AW
import CSVParser (RankingCSV, parseRankingCSV)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (any, elem, find, null, sortBy)
import Data.Array (foldl) as Array
import Data.Either (Either(..))
import Data.Int (trunc)
import Data.Int as DI
import Effect.Aff.Class (class MonadAff)
import Effect.Console as CONSOLE
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Elements (button)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as H
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Number as DN
import Data.String (trim)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Player
import Sorting

data Query a = GetState (State -> a)

type State = {
    allPlayers :: Map String Player
  , players :: Map String Player
  , filterInputs :: Array String
  , currentSort :: SortOption
  , loading :: Boolean
  , error :: Maybe String
  , sortChangeFlag :: Boolean
}

type PosLookup = Tuple String String 

position :: Array PosLookup
position = 
  [ Tuple "All" ""
  , Tuple "P" "1"
  , Tuple "C" "2"
  , Tuple "1B" "3"
  , Tuple "2B" "4"
  , Tuple "3B" "5"
  , Tuple "SS" "6"
  , Tuple "LF" "7"
  , Tuple "CF" "8"
  , Tuple "RF" "9"
  , Tuple "DH" "10"
  , Tuple "PH" "11"
  , Tuple "PR" "12"
  , Tuple "UN" "13"
  , Tuple "O" "O"
  , Tuple "Switch" "Y"
  ]

type TeamLookup = Tuple String Int 

teams :: Array TeamLookup
teams = 
  [ Tuple "LAA" 108
  , Tuple "ARI" 109
  , Tuple "BAL" 110
  , Tuple "BOS" 111
  , Tuple "CHC" 112
  , Tuple "CIN" 113
  , Tuple "CLE" 114
  , Tuple "COL" 115
  , Tuple "DET" 116
  , Tuple "HOU" 117
  , Tuple "KC" 118
  , Tuple "LAD" 119
  , Tuple "WSH" 120
  , Tuple "NYM" 121
  , Tuple "OAK" 133
  , Tuple "PIT" 134
  , Tuple "SD" 135
  , Tuple "SEA" 136
  , Tuple "SF" 137
  , Tuple "STL" 138
  , Tuple "TB" 139
  , Tuple "TEX" 140
  , Tuple "TOR" 141
  , Tuple "MIN" 142
  , Tuple "PHI" 143
  , Tuple "ATL" 144
  , Tuple "CWS" 145
  , Tuple "MIA" 146
  , Tuple "NYY" 147
  , Tuple "MIL" 158
  ]


initialState :: State
initialState = {
    allPlayers: Map.empty
  , players: Map.empty
  , filterInputs: []
  , currentSort: "ID" 
  , loading: true
  , error: Nothing
  , sortChangeFlag: false
}

data Action
  = TogglePositionFilter String
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
    H.liftEffect $ CONSOLE.log "Component initializing..."
    eitherPlayersMap <- liftAff fetchPlayers
    eitherRankings <- liftAff fetchRankings
    case (Tuple eitherPlayersMap eitherRankings) of
      (Tuple Right playersMap Right rankings) -> do
        let defaultSort = "ID"
        let mergedAndSortedPlayers = mergeAndSortPlayers playersMap rankings defaultSort
        H.modify_ \s -> s { allPlayers = mergedAndSortedPlayers, players = mergedAndSortedPlayers, loading = false }
        H.liftEffect $ CONSOLE.log "Data successfully initialized, merged, and sorted"
      _ -> do
        let errMsg = either (\err -> err) (const "Unknown error") eitherPlayersMap
        H.modify_ \s -> s { error = Just errMsg, loading = false }


  DataFetched playersMap rankings -> do
    oldState <- H.get
    let defaultSort = "ID" 
    let mergedAndSortedPlayers = mergeAndSortPlayers playersMap rankings defaultSort
    let newState = updatePlayersView $ oldState { allPlayers = mergedAndSortedPlayers, players = mergedAndSortedPlayers }
    H.put newState

  TogglePositionFilter posCode -> do
    oldState <- H.get
    let newFilters = toggleFilter posCode oldState.filterInputs
    let filteredPlayers = filterActivePlayers newFilters oldState.allPlayers
    let newState = oldState { filterInputs = newFilters, players = filteredPlayers }
    H.put newState

  SortBy newSort -> do
    H.liftEffect $ CONSOLE.log $ "Sorting by: " <> newSort
    oldState <- H.get
    let newState = updatePlayersView $ oldState { currentSort = newSort }
    H.put newState

  HandleError errorMsg -> 
    H.modify_ \s -> s { error = Just errorMsg, loading = false }

updatePlayersView :: State -> State
updatePlayersView currentState = 
  let 
    filteredPlayers = filterActivePlayers currentState.filterInputs currentState.allPlayers
    sortedFilteredPlayers = sortBySelectedOption currentState.currentSort filteredPlayers
  in 
    currentState { players = sortedFilteredPlayers }

toggleFilter :: forall a. Eq a => a -> Array a -> Array a
toggleFilter x arr = if elem x arr then arr \\ [x] else arr <> [x]

positionButtons :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
positionButtons state = 
  HH.div_
    $ map createButton position
  where
    createButton :: (Tuple String String) -> H.ComponentHTML Action () m
    createButton (Tuple posName posCode) =
      let action = TogglePositionFilter posCode
      in
        HH.button
          [ HP.type_ ButtonButton
          , HE.onClick $ \_ -> H.action
          , HP.classes $ if elem posCode state.filterInputs then ["active"] else []
          ]
          [ HH.text posName ]

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =   
  HH.div_
    [ positionButtons state
    , sortDropdown state.currentSort
    , playersTable state.players
    ]

columnNames :: Array String
columnNames = ["PlayerID ", "First ", "Last ", "Team ", "Pitch ", "Bat ", "Pos ", "Active ", "'23 Rank ", "'23 Points ", "NameSlug "]

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
    , HH.td [ CSS.style cellStyle ] [ HH.text $ maybe "0" show player.past_ranking ]  
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

isPosition :: String -> Player -> Boolean
isPosition posCode player = player.primaryPosition == posCode

sortDropdown :: forall m. MonadAff m => SortOption -> H.ComponentHTML Action () m
sortDropdown currentSort = 
  HH.select
    [ HE.onValueInput (SortBy) ]
    $ map (\option -> HH.option
                      [ HP.value option
                      , H.selected $ option == currentSort
                      ] [ HH.text option ]) sortOptions

mergeAndSortPlayers :: Map String Player -> RankingCSV -> SortOption -> Map String Player
mergeAndSortPlayers playersMap csvData defaultSort = 
  let
    mergedPlayers = mergePlayerData playersMap csvData
  in
    sortBySelectedOption defaultSort mergedPlayers

getDisplayValue :: String -> String
getDisplayValue value =
  maybe value fst (find (\(Tuple _ code) -> code == value) position)

getTeamDisplayValue :: Int -> String
getTeamDisplayValue value =
  maybe (show value) fst (find (\(Tuple _ code) -> code == value) teams)

showAsInt :: Number -> String
showAsInt num = show $ trunc num

fetchPlayers :: Aff (Either String (Map String Player))
fetchPlayers = do
  response <- AW.request $ defaultRequest
    { url = "./appData/rosters/activePlayers.json"
    , responseFormat = json
    }
  case response of
    Left err -> do
      let errorMsg = "Fetch Error: " <> AW.printError err
      pure $ Left errorMsg

    Right res -> do
      case decodeJson res.body of
        Right (ActivePlayers (PlayersMap playersMap)) -> do
          pure $ Right playersMap
        Left decodeError -> do
          let errorMsg = "Decode Error: " <> printJsonDecodeError decodeError
          pure $ Left errorMsg

filterActivePlayers :: Array String -> Map String Player -> Map String Player
filterActivePlayers posCodes playersMap =
  case posCodes of
    [] -> playersMap 
    codes -> Map.filter (\player -> elem player.primaryPosition codes) playersMap

fetchRankings :: Aff (Either String RankingCSV)
fetchRankings = do
  response <- AW.request $ defaultRequest 
    { url = "./appData/rosters/2023_Rankings.csv"
    , responseFormat = string 
    }
  case response of
    Left err -> pure $ Left $ AW.printError err
    Right res -> pure $ Right $ parseRankingCSV res.body

mergePlayerData :: Map String Player -> RankingCSV -> Map String Player
mergePlayerData playersMap csvData = Array.foldl updatePlayerRanking playersMap csvData
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
