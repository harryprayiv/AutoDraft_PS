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
import Data.Either (Either(..))
import Data.Int as Data.Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Data.Number
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen (liftAff)
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
  = HandleInput String
  | Initialize
  | FilterPlayers
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
      [ inputField state.positionInput,
        HH.button 
          [ HE.onClick $ const FilterPlayers
          , HP.disabled state.loading
          ] 
          [ HH.text (if state.loading then "Loading..." else "Filter Players") ]
      ]
    , playersTable state.players
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  HandleInput input -> 
    H.modify_ \s -> s { positionInput = input }
  
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
        let mergedPlayers = mergePlayerData playersMap rankings
        H.modify_ \s -> s { allPlayers = mergedPlayers, players = playersMap, loading = false }
        H.liftEffect $ Console.log "Data successfully initialized and merged"

  FilterPlayers -> do
    positionInput <- H.gets _.positionInput
    allPlayersMap <- H.gets _.allPlayers
    let filteredPlayers = filterActivePlayers positionInput allPlayersMap
    H.modify_ \s -> s { players = filteredPlayers }

  HandleError errorMsg -> 
    H.modify_ \s -> s { error = Just errorMsg, loading = false }

inputField :: forall m. MonadAff m => String -> H.ComponentHTML Action () m
inputField inputValue = 
  HH.input [ HP.type_ HP.InputText, HP.value inputValue, HE.onValueInput HandleInput ]

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
      <> " | FPTS: "
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

mergePlayerData :: Map String Player -> CSV -> Map String Player
mergePlayerData playersMap csvData =
  Array.foldl updatePlayerRanking playersMap csvData
  where
    updatePlayerRanking acc row =
      case row of
        [mlbId, _, _, fptsStr, rankingStr] ->
          let
            maybeRanking = parseToInt rankingStr
            maybeFPTS = Data.Number.fromString fptsStr
            updatePlayer player = player 
              { past_ranking = maybeRanking
              , past_fpts = maybeFPTS
              }
          in Map.update (Just <<< updatePlayer) mlbId acc
        _ -> acc

parseToInt :: String -> Maybe Int
parseToInt str = Data.Int.fromString str