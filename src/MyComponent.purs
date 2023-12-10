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
import Player (ActivePlayers(..), Player, PlayersMap(..), filterPlayers)
data Query a = GetState (State -> a)

type State = 
  { players :: Map String Player
  , positionInput :: String
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = HandleInput String
  | Submit
  | LoadPlayers
  | SetPlayers (Either String (Map String Player))
  | HandleError String

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: State
initialState = 
  { players: Map.empty
  , positionInput: ""
  , loading: false
  , error: Nothing
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =    
  HH.div_
    [ inputField state.positionInput,
      submitButton state.loading,
      maybeElem state.error errorDiv,
      playersTable state.players
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput input -> 
    H.modify_ \s -> s { positionInput = input }
    
  Submit -> do
    positionInput <- H.gets _.positionInput
    H.liftEffect $ Console.log $ "Position Input: " <> positionInput
    H.modify_ \s -> s { loading = true }
    result <- liftAff $ fetchAndFilterPlayers positionInput
    case result of
      Left errorMsg -> do
        H.liftEffect $ Console.log $ "Error: " <> errorMsg
        H.modify_ \s -> s { error = Just errorMsg, loading = false }
      Right playersMap -> do
        let filteredPlayers = filterPlayers positionInput playersMap
        H.liftEffect $ Console.log $ "Filtered Players: " <> show (Map.size filteredPlayers)

  LoadPlayers -> do
    positionInput <- H.gets _.positionInput
    H.modify_ \s -> s { loading = true }
    result <- liftAff $ fetchAndFilterPlayers positionInput
    case result of
      Left errorMsg ->
        H.modify_ \s -> s { error = Just errorMsg, loading = false }
      Right playersMap ->
        H.modify_ \s -> s { players = playersMap, loading = false, error = Nothing }

  SetPlayers result ->
    case result of
      Right playersMap ->
        H.modify_ \s -> s { players = playersMap, loading = false, error = Nothing }
      Left errorMsg ->
        H.modify_ \s -> s { error = Just errorMsg, loading = false }
  HandleError errorMsg -> 
    H.modify_ \s -> s { error = Just errorMsg, loading = false }

maybeElem :: forall w i a. Maybe a -> (a -> HH.HTML w i) -> HH.HTML w i
maybeElem val f =
  case val of
    Just x -> f x
    Nothing -> HH.text ""

inputField :: forall m. MonadAff m => String -> H.ComponentHTML Action () m
inputField inputValue = 
  HH.input [ HP.type_ HP.InputText, HP.value inputValue, HE.onValueInput HandleInput ]

submitButton :: forall m. MonadAff m => Boolean -> H.ComponentHTML Action () m
submitButton loading = 
  HH.button [ HP.disabled loading, HE.onClick $ const Submit ] [ HH.text (if loading then "Loading..." else "Load Players") ]

errorDiv :: forall m. MonadAff m => String -> H.ComponentHTML Action () m
errorDiv errorMsg = HH.div_ [ HH.text errorMsg ]

playersTable :: forall m. MonadAff m => Map String Player -> H.ComponentHTML Action () m
playersTable players = HH.div_ $ map renderPlayer $ Map.toUnfoldable players

renderPlayer :: forall m. MonadAff m => Tuple String Player -> H.ComponentHTML Action () m
renderPlayer (Tuple _ player) = HH.div_ [ HH.text $ player.useName <> " - Position: " <> player.primaryPosition ]

fetchAndFilterPlayers :: String -> Aff (Either String (Map String Player))
fetchAndFilterPlayers positionInput = do
  let request = defaultRequest { url = "./appData/rosters/activePlayers.json", responseFormat = json }
  response <- AW.request request

  case response of
    Left err -> do
      let errorMsg = "Fetch Error: " <> AW.printError err
      H.liftEffect $ Console.log errorMsg
      pure $ Left errorMsg

    Right res -> do
      H.liftEffect $ Console.log $ "Raw JSON Response: " <> stringify res.body
      case decodeJson res.body of
        Right (ActivePlayers (PlayersMap playersMap)) -> do
          let filteredPlayers = filterPlayers positionInput playersMap
          H.liftEffect $ Console.log $ "Filtered Players: " <> show filteredPlayers
          pure $ Right filteredPlayers
        Left decodeError -> do
          let errorMsg = "Decode Error: " <> printJsonDecodeError decodeError
          H.liftEffect $ Console.log errorMsg
          pure $ Left errorMsg

