module MyComponent
  ( component
  )
  where

import Prelude

import Affjax (defaultRequest)
import Affjax.ResponseFormat (json, string)
import Affjax.Web as AW
import CSVParser (CSV, parseCSV)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (foldl) as Array
import Data.Array (sortBy)
import Data.Either (Either(..))
import Data.Int as Data.Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Data.Number
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen (liftAff)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Player (ActivePlayers(..), Player, PlayersMap(..))

data Query a = GetState (State -> a)

type State = 
  { allPlayers :: Map String Player
  , players :: Map String Player
  , positionInput :: String
  , loading :: Boolean
  , error :: Maybe String
  }

initialState :: State
initialState = 
  { allPlayers: Map.empty
  , players: Map.empty
  , positionInput: ""
  , loading: false
  , error: Nothing
  }

data Action
  = FilterByPosition String
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
    [ HH.div_ 
        [ positionButton state.loading "P" "1"
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
        ]
    , playersTable state.players
    ]
    
positionButton :: forall m. MonadAff m => Boolean -> String -> String -> H.ComponentHTML Action () m
positionButton loading displayName positionCode = 
  HH.button 
    [ HE.onClick $ const $ FilterByPosition positionCode
    , HP.disabled loading
    ] 
    [ HH.text displayName ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  -- HandleInput input -> 
  --   H.modify_ \s -> s { positionInput = input }

  Initialize -> do
    H.liftEffect $ Console.log "Component initializing..."
    playerResult <- liftAff fetchPlayers
    rankingResult <- liftAff fetchRankings

    case Tuple playerResult rankingResult of
      Tuple (Left playerError) _ -> do
        H.liftEffect $ Console.log $ "Error fetching player data: " <> playerError
        H.modify_ \s -> s { error = Just playerError, loading = false }

      Tuple _ (Left rankingError) -> do
        H.liftEffect $ Console.log $ "Error fetching ranking data: " <> rankingError
        H.modify_ \s -> s { error = Just rankingError, loading = false }

      Tuple (Right playersMap) (Right rankings) -> do
        let mergedAndSortedPlayers = sortPlayers $ mergePlayerData playersMap rankings
        H.modify_ \s -> s { allPlayers = mergedAndSortedPlayers, players = mergedAndSortedPlayers, loading = false }
        H.liftEffect $ Console.log "Data successfully initialized, merged, and sorted"

  FilterByPosition position -> do
    allPlayersMap <- H.gets _.allPlayers
    let filteredPlayers = filterActivePlayers position allPlayersMap
    let sortedFilteredPlayers = sortPlayers filteredPlayers
    H.modify_ \s -> s { players = sortedFilteredPlayers }

  HandleError errorMsg -> 
    H.modify_ \s -> s { error = Just errorMsg, loading = false }


-- inputField :: forall m. MonadAff m => String -> H.ComponentHTML Action () m
-- inputField inputValue = 
--   HH.input [ HP.type_ HP.InputText, HP.value inputValue, HE.onValueInput HandleInput ]

playersTable :: forall m. MonadAff m => Map String Player -> H.ComponentHTML Action () m
playersTable players = 
  HH.div_ $ map renderPlayer $ Map.toUnfoldable players

-- Update the renderPlayer function to display rankings and FPTS if available
renderPlayer :: forall m. MonadAff m => Tuple String Player -> H.ComponentHTML Action () m
renderPlayer (Tuple _ player) = 
  HH.div_ 
    [ HH.text $ player.useName 
      <> " " 
      <> player.useLastName 
      <> " | Position: " 
      <> player.primaryPosition 
      <> " | Active: " 
      <> show player.active
      <> " | 2023 Total: "
      <> (fromMaybe "N/A" (show <$> player.past_fpts))
      <> " | Ranking: "
      <> (fromMaybe "Unranked" (show <$> player.past_ranking))
    ]

fetchPlayers :: Aff (Either String (Map String Player))
fetchPlayers = do
  response <- AW.request $ defaultRequest
    { url = "./appData/rosters/activePlayers.json"
    , responseFormat = json
    }

  case response of
    Left err -> do
      let errorMsg = "Fetch Error: " <> AW.printError err
      H.liftEffect $ Console.log errorMsg
      pure $ Left errorMsg

    Right res -> do
      H.liftEffect $ Console.log $ "Raw JSON Response: " <> stringify res.body
      case decodeJson res.body of
        Right (ActivePlayers (PlayersMap playersMap)) -> do
          H.liftEffect $ Console.log $ "All Players: " <> show (Map.size playersMap)
          pure $ Right playersMap
        Left decodeError -> do
          let errorMsg = "Decode Error: " <> printJsonDecodeError decodeError
          H.liftEffect $ Console.log errorMsg
          pure $ Left errorMsg

filterActivePlayers :: String -> Map String Player -> Map String Player
filterActivePlayers positionInput playersMap =
    if positionInput == "" then playersMap -- If no input, return all players
    else Map.filter (\player -> player.primaryPosition == positionInput) playersMap

fetchRankings :: Aff (Either String CSV)
fetchRankings = do
  response <- AW.request $ defaultRequest 
    { url = "./appData/rosters/2023_Rankings.csv"
    , responseFormat = string 
    }

  case response of
    Left err -> pure $ Left $ AW.printError err
    Right res -> pure $ Right $ parseCSV res.body

-- Ensure this function is parsing the ranking and FPTS correctly
mergePlayerData :: Map String Player -> CSV -> Map String Player
mergePlayerData playersMap csvData = Array.foldl updatePlayerRanking playersMap csvData
  where
    updatePlayerRanking acc row = case row of
      [mlbId, _, _, fptsStr, rankingStr] -> 
        let
          maybeRanking = Data.Number.fromString rankingStr
          maybeFPTS = Data.Number.fromString fptsStr
          updatePlayer player = player 
            { past_ranking = maybeRanking
            , past_fpts = maybeFPTS }
        in Map.update (Just <<< updatePlayer) mlbId acc
      _ -> acc

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


parseToInt :: String -> Maybe Int
parseToInt str = Data.Int.fromString str