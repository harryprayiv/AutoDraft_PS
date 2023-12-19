module MyComponent
  ( component
  )
  where

import Prelude

import Affjax (defaultRequest)
import Affjax.ResponseFormat (json, string)
import Affjax.Web as AW
import CSS.Border as Border
import CSS.Color as Color
import CSS.Geometry (paddingBottom, paddingLeft, paddingRight, paddingTop) as CSS
import CSS.Size as Size
import CSS.Stylesheet (CSS)
import CSVParser (RankingCSV, parseRankingCSV)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (foldl) as Array
import Data.Array (sortBy)
import Data.Either (Either(..))
import Data.Int (trunc)
import Data.Int as DI
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Number as DN
import Data.String (trim)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as DEBUG
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Player (ActivePlayers(..), Player, PlayersMap(..))

data Query a = GetState (State -> a)

filterDropdown :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
filterDropdown state = 
  HH.select
    [ HE.onValueInput (FilterByPosition) ]
    $ map (\(Tuple displayName positionCode) -> HH.option
           [ HP.value positionCode
           , HP.selected $ positionCode == state.filterInput
           ] [ HH.text displayName ]) positionOptions

type State = 
  { allPlayers :: Map String Player
  , players :: Map String Player
  , filterInput :: String
  , currentSort :: SortOption
  , loading :: Boolean
  , error :: Maybe String
  , sortChangeFlag :: Boolean
  }

initialState :: State
initialState = 
  { allPlayers: Map.empty
  , players: Map.empty
  , filterInput: ""
  , currentSort: "Name"
  , loading: false
  , error: Nothing
  , sortChangeFlag : false
  }

data Action
  = FilterByPosition String
  | ChangeSort SortOption
  | Initialize
  | HandleError String
  
component :: forall q i m. MonadAff m => H.Component q i Void m
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
    [ filterDropdown state
    , sortDropdown state.currentSort
    , playersTable state.players
    ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Void m Unit
handleAction = case _ of
  ChangeSort newSort -> do
    H.modify_ \s -> s { currentSort = newSort }
    sortPlayersBySelectedOption newSort
    H.liftEffect $ DEBUG.log $ "New Sorting Chosen" <> show newSort

  Initialize -> do
    H.liftEffect $ DEBUG.log "Component initializing..."
    playerResult <- liftAff fetchPlayers
    rankingResult <- liftAff fetchRankings
    H.liftEffect $ DEBUG.log $ "Parsed Rankings: " <> show rankingResult

    case Tuple playerResult rankingResult of
      Tuple (Left playerError) _ -> do
        H.liftEffect $ DEBUG.log $ "Error fetching player data: " <> playerError
        H.modify_ \s -> s { error = Just playerError, loading = false }

      Tuple _ (Left rankingError) -> do
        H.liftEffect $ DEBUG.log $ "Error fetching ranking data: " <> rankingError
        H.modify_ \s -> s { error = Just rankingError, loading = false }

      Tuple (Right playersMap) (Right rankings) -> do
        let mergedPlayers = mergePlayerData playersMap rankings
        let mergedAndSortedPlayers = sortByMLBid mergedPlayers
        H.modify_ \s -> s { allPlayers = mergedAndSortedPlayers, players = mergedAndSortedPlayers, loading = true }
        H.liftEffect $ DEBUG.log "Data successfully initialized, merged, and sorted"

  FilterByPosition position -> do
    allPlayersMap <- H.gets _.allPlayers
    currentSortOption <- H.gets _.currentSort
    let filteredPlayers = filterActivePlayers position allPlayersMap
    let sortedFilteredPlayers = applyCurrentSortOption currentSortOption filteredPlayers
    H.modify_ \s -> s { players = sortedFilteredPlayers }
    H.liftEffect $ DEBUG.log "Data filtered and sorted"

  HandleError errorMsg -> 
    H.modify_ \s -> s { error = Just errorMsg, loading = false }

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
    , HH.td [ CSS.style cellStyle ] [ HH.text $ show player.currentTeam ]
    , HH.td [ CSS.style cellStyle ] [ HH.text player.pitchHand ]
    , HH.td [ CSS.style cellStyle ] [ HH.text player.batSide ]   
    , HH.td [ CSS.style cellStyle ] [ HH.text player.primaryPosition ] 
    , HH.td [ CSS.style cellStyle ] [ HH.text $ show player.active ]         
    , HH.td [ CSS.style cellStyle ] [ HH.text $ maybe "N/A" show player.past_ranking ]  
    , HH.td [ CSS.style cellStyle ] [ HH.text $ maybe "N/A" showAsInt player.past_fpts ]  
    , HH.td [ CSS.style cellStyle ] [ HH.text player.nameSlug ] 
    ]

playersTable :: forall m. MonadAff m => Map String Player -> H.ComponentHTML Action () m
playersTable players = 
  HH.table_
    [ HH.thead_ 
        [ HH.tr_ $ map (\name -> HH.th_ [HH.text name]) columnNames ]
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

type PositionOption = Tuple String String 

positionOptions :: Array PositionOption
positionOptions = 
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
  , Tuple "O" "O"
  , Tuple "Shohei" "Y"
  ]

filterActivePlayers :: String -> Map String Player -> Map String Player
filterActivePlayers filterInput playersMap =
    if filterInput == "" then playersMap
    else Map.filter (\player -> player.primaryPosition == filterInput) playersMap

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

-- Sorting functionality
type SortOption = String

sortOptions :: Array SortOption
sortOptions = ["MLB ID (default)", "Surname", "'23 Rank", "'23 Points"]

sortDropdown :: forall m. MonadAff m => SortOption -> H.ComponentHTML Action () m
sortDropdown currentSort = 
  HH.select
    [ HE.onValueInput ChangeSort ]
    $ map (\option -> HH.option
                      [ HP.value option
                      , HP.selected $ option == currentSort
                      ] [ HH.text option ]) sortOptions

sortByMLBid :: Map String Player -> Map String Player
sortByMLBid = sortPlayersBy (\player -> Just player.playerId)

sortBySurname :: Map String Player -> Map String Player
sortBySurname = sortPlayersBy (\player -> Just (player.useLastName <> " " <> player.useName))

sortByRank :: Map String Player -> Map String Player
sortByRank = sortPlayersBy (\player -> player.past_ranking) 

sortByPoints :: Map String Player -> Map String Player
sortByPoints = sortPlayersBy (\player -> player.past_fpts)

sortPlayersBySelectedOption :: forall m. MonadAff m => SortOption -> H.HalogenM State Action () Void m Unit
sortPlayersBySelectedOption sortOption = do
  currentPlayersMap <- H.gets _.players
  H.liftEffect $ DEBUG.log $ "Current Players (before sorting): " <> show currentPlayersMap
  let sortedPlayers = case sortOption of
                         "MLB ID (default)" -> sortByMLBid currentPlayersMap
                         "Surname" -> sortBySurname currentPlayersMap
                         "'23 Rank" -> sortByRank currentPlayersMap
                         "'23 Points" -> sortByPoints currentPlayersMap
                         _ -> currentPlayersMap
  H.modify_ \s -> s { players = sortedPlayers }
  sortedPlayersMap <- H.gets _.players
  H.liftEffect $ DEBUG.log $ "Sorted Players (after sorting): " <> show sortedPlayersMap

applyCurrentSortOption :: SortOption -> Map String Player -> Map String Player
applyCurrentSortOption sortOption playersMap = 
  case sortOption of
    "MLB ID (default)" -> sortByMLBid playersMap
    "Surname" -> sortBySurname playersMap
    "'23 Rank" -> sortByRank playersMap
    "'23 Points" -> sortByPoints playersMap
    _ -> playersMap

sortPlayersBy :: forall a. Ord a => (Player -> Maybe a) -> Map String Player -> Map String Player
sortPlayersBy f playersMap =
  let
    comparePlayers t1 t2 = compareMaybe (f $ snd t1) (f $ snd t2)
  in
    Map.fromFoldable $ sortBy comparePlayers $ Map.toUnfoldable playersMap

compareMaybe :: forall a. Ord a => Maybe a -> Maybe a -> Ordering
compareMaybe (Just a) (Just b) = compare a b
compareMaybe Nothing (Just _)  = GT 
compareMaybe (Just _) Nothing  = LT
compareMaybe Nothing Nothing   = EQ