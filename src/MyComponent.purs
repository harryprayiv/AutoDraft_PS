module MyComponent
  ( component
  )
  where

import Prelude

import Affjax (defaultRequest)
import Affjax.ResponseFormat (json)
import Affjax.Web as AW
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
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
    result <- liftAff fetchPlayers
    case result of
      Left errorMsg -> do
        H.liftEffect $ Console.log $ "Error: " <> errorMsg
        H.modify_ \s -> s { error = Just errorMsg, loading = false }
      Right playersMap -> do
        H.modify_ \s -> s { allPlayers = playersMap, players = playersMap, loading = false }
        H.liftEffect $ Console.log $ "Active Players: " <> show (Map.size playersMap)

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

renderPlayer :: forall m. MonadAff m => Tuple String Player -> H.ComponentHTML Action () m
renderPlayer (Tuple _ player) = 
  HH.div_ 
    [ HH.text $ player.useName 
      <> " " 
      <> player.useLastName 
      <> " | Position: " 
      <> player.primaryPosition 
      <> " | Active: " 
      <> show player.active -- Convert Boolean to String
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
