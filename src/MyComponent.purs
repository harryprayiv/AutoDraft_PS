module MyComponent
  ( component
  )
  where

import Prelude
import Affjax (defaultRequest)
import Affjax.ResponseFormat (json, string)
import CSS.Stylesheet (CSS)
import CSVParser (RankingCSV, parseRankingCSV)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (sortBy)
import Data.Either (Either(..))
import Data.Int (trunc)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftAff)
import Data.Map as Map
import Data.Number as DN
import Data.Array (foldl) as Array
import Data.Int as DI
import Affjax.Web as AW
import CSS.Border as Border
import CSS.Color as Color
import CSS.Size as Size
import Effect.Console as DEBUG
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


import Player (ActivePlayers(..), Player, PlayersMap(..))

data Query a = GetState (State -> a)

type SortOption = String

sortOptions :: Array SortOption
sortOptions = ["Name", "'23 Rank", "'23 Points"]

type State = 
  { allPlayers :: Map String Player
  , players :: Map String Player
  , positionInput :: String
  , currentSort :: SortOption
  , loading :: Boolean
  , error :: Maybe String
  }

initialState :: State
initialState = 
  { allPlayers: Map.empty
  , players: Map.empty
  , positionInput: ""
  , currentSort: "Name" -- Default sort option
  , loading: false
  , error: Nothing
  }

data Action
  = FilterByPosition String
  | ChangeSort SortOption
  | Initialize
  | HandleError String
  
component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =    
  HH.div_
    [ positionButton state.loading "All" ""
    , positionButton state.loading "P" "1"
    , positionButton state.loading "C" "2"
    , positionButton state.loading "1B" "3"
    , positionButton state.loading "2B" "4"
    , positionButton state.loading "3B" "5"
    , positionButton state.loading "SS" "6"
    , positionButton state.loading "LF" "7"
    , positionButton state.loading "CF" "8"
    , positionButton state.loading "RF" "9"
    , positionButton state.loading "DH" "10"
    , positionButton state.loading "O" "O"
    , positionButton state.loading "Shohei" "Y"
    , sortDropdown state.currentSort
    , playersTable state.players
    ]

sortDropdown :: forall m. MonadAff m => SortOption -> H.ComponentHTML Action () m
sortDropdown currentSort = 
  HH.select
    [ HE.onValueInput ChangeSort ]
    $ map (\option -> HH.option
                      [ HP.value option
                      , HP.selected $ option == currentSort
                      ] [ HH.text option ]) sortOptions

positionButton :: forall m. MonadAff m => Boolean -> String -> String -> H.ComponentHTML Action () m
positionButton loading displayName positionCode = 
  HH.button 
    [ HE.onClick $ const $ FilterByPosition positionCode
    , HP.disabled loading
    ] 
    [ HH.text displayName ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  ChangeSort newSort -> do
    H.modify_ \s -> s { currentSort = newSort }
    sortPlayersBySelectedOption newSort
  Initialize -> do
    H.liftEffect $ DEBUG.log "Component initializing..."
    playerResult <- liftAff fetchPlayers
    rankingResult <- liftAff fetchRankings

    case Tuple playerResult rankingResult of
      Tuple (Left playerError) _ -> do
        H.liftEffect $ DEBUG.log $ "Error fetching player data: " <> playerError
        H.modify_ \s -> s { error = Just playerError, loading = false }

      Tuple _ (Left rankingError) -> do
        H.liftEffect $ DEBUG.log $ "Error fetching ranking data: " <> rankingError
        H.modify_ \s -> s { error = Just rankingError, loading = false }

      Tuple (Right playersMap) (Right rankings) -> do
        let mergedAndSortedPlayers = sortPlayers $ mergePlayerData playersMap rankings
        H.modify_ \s -> s { allPlayers = mergedAndSortedPlayers, players = mergedAndSortedPlayers, loading = false }
        H.liftEffect $ DEBUG.log "Data successfully initialized, merged, and sorted"

  FilterByPosition position -> do
    allPlayersMap <- H.gets _.allPlayers
    let filteredPlayers = filterActivePlayers position allPlayersMap
    let sortedFilteredPlayers = sortPlayers filteredPlayers
    H.modify_ \s -> s { players = sortedFilteredPlayers }

  HandleError errorMsg -> 
    H.modify_ \s -> s { error = Just errorMsg, loading = false }


cellStyle :: CSS
cellStyle = do
  Border.border Border.solid (Size.px 1.0) Color.black

renderPlayer :: forall m. MonadAff m => Tuple String Player -> H.ComponentHTML Action () m
renderPlayer (Tuple _ player) = 
  HH.tr_
    [ HH.td [CSS.style cellStyle] [HH.text player.useName]
    , HH.td [ CSS.style cellStyle ] [ HH.text player.useLastName ]
    , HH.td [ CSS.style cellStyle ] [ HH.text $ show player.currentTeam ]
    , HH.td [ CSS.style cellStyle ] [ HH.text player.pitchHand ]
    , HH.td [ CSS.style cellStyle ] [ HH.text player.batSide ]   
    , HH.td [ CSS.style cellStyle ] [ HH.text player.primaryPosition ] 
    , HH.td [ CSS.style cellStyle ] [ HH.text $ show player.active ]         
    , HH.td [ CSS.style cellStyle ] [ HH.text $ maybe "Unranked" show player.past_ranking ]  
    , HH.td [ CSS.style cellStyle ] [ HH.text $ maybe "N/A" showAsInt player.past_fpts ]  
    ]

playersTable :: forall m. MonadAff m => Map String Player -> H.ComponentHTML Action () m
playersTable players = 
  HH.table_
    [ HH.thead_ [ HH.tr_ [ HH.th_ [HH.text "Header Name"] ] ]
    , HH.tbody_ $ map renderPlayer $ Map.toUnfoldable players
    ]

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
      H.liftEffect $ DEBUG.log errorMsg
      pure $ Left errorMsg

    Right res -> do
      H.liftEffect $ DEBUG.log $ "Raw JSON Response: " <> stringify res.body
      case decodeJson res.body of
        Right (ActivePlayers (PlayersMap playersMap)) -> do
          H.liftEffect $ DEBUG.log $ "All Players: " <> show (Map.size playersMap)
          pure $ Right playersMap
        Left decodeError -> do
          let errorMsg = "Decode Error: " <> printJsonDecodeError decodeError
          H.liftEffect $ DEBUG.log errorMsg
          pure $ Left errorMsg

filterActivePlayers :: String -> Map String Player -> Map String Player
filterActivePlayers positionInput playersMap =
    if positionInput == "" then playersMap
    else Map.filter (\player -> player.primaryPosition == positionInput) playersMap

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
          maybeRanking = DI.fromString rankingStr
          maybeFPTS = DN.fromString fptsStr
          updatePlayer player = player
            { past_ranking = if isNothing maybeRanking then player.past_ranking else maybeRanking
            , past_fpts = if isNothing maybeFPTS then player.past_fpts else maybeFPTS }
        in Map.update (Just <<< updatePlayer) mlbId acc
      _ -> acc

sortPlayersBySelectedOption :: forall output m. MonadAff m => SortOption -> H.HalogenM State Action () output m Unit
sortPlayersBySelectedOption sortOption = do
  currentPlayersMap <- H.gets _.players
  let sortedPlayers = case sortOption of
        "Name" -> sortPlayersBy (\p -> p.useLastName <> " " <> p.useName) currentPlayersMap
        "'23 Rank" -> sortPlayersBy (fromMaybe 0 <<< _.past_ranking) currentPlayersMap
        "'23 Points" -> sortPlayersBy (fromMaybe 0.0 <<< _.past_fpts) currentPlayersMap
        _ -> currentPlayersMap
  H.modify_ \s -> s { players = sortedPlayers }

sortPlayersBy :: forall a. Ord a => (Player -> a) -> Map String Player -> Map String Player
sortPlayersBy f playersMap =
  let
    comparePlayers :: Tuple String Player -> Tuple String Player -> Ordering
    comparePlayers t1 t2 = compare (f $ snd t1) (f $ snd t2)
  in
    Map.fromFoldable $ sortBy comparePlayers $ Map.toUnfoldable playersMap

sortPlayers :: Map String Player -> Map String Player
sortPlayers playersMap =
  let
    comparePlayers :: Tuple String Player -> Tuple String Player -> Ordering
    comparePlayers t1 t2 =
      let
        ranking1 = (snd t1).past_ranking
        ranking2 = (snd t2).past_ranking
        name1 = (snd t1).useName
        name2 = (snd t2).useName
        compareRanking (Just r1) (Just r2) = compare r1 r2
        compareRanking (Just _) Nothing = LT
        compareRanking Nothing (Just _) = GT
        compareRanking Nothing Nothing = compare name1 name2
      in
        compareRanking ranking1 ranking2
  in
    Map.fromFoldable $ sortBy comparePlayers $ Map.toUnfoldable playersMap
