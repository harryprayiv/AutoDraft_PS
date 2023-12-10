module MyComponent
  ( component
  )
  where

import Player
import Prelude

import Affjax (AffjaxDriver, get, request, defaultRequest, printError, Response)
import Affjax.ResponseFormat (json)
import Affjax.Web as AW
import Data.Argonaut.Core (Json, jsonEmptyObject, toObject, jsonNull)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object, lookup)
import Foreign.Object as Object
import Halogen (HalogenM, liftAff)
import Halogen as H
import Halogen.Aff.Util (runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

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
    { initialState: \_ -> initialState  -- Ensure initialState is correctly used
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
    H.modify_ \s -> s { loading = true }
    result <- liftAff $ fetchAndFilterPlayers positionInput
    case result of
      Left errorMsg ->
        H.modify_ \s -> s { error = Just errorMsg, loading = false }
      Right playersMap ->
        H.modify_ \s -> s { players = playersMap, loading = false, error = Nothing }

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

filterPlayers :: String -> Map String Player -> Map String Player
filterPlayers positionInput players = 
  Map.filter (\p -> show p.primaryPosition == positionInput) players

fetchAndFilterPlayers :: String -> Aff (Either String (Map String Player))
fetchAndFilterPlayers positionInput = do
  response <- AW.request $ defaultRequest
    { url = "./appData/rosters/activePlayers.json"
    , responseFormat = json
    }
  pure $ case response of
    Left err -> Left $ "Error loading JSON: " <> printError err
    Right res -> case decodeJson res.body of
      Right (ActivePlayers (PlayersMap playersMap)) ->
        Right $ filterPlayers positionInput playersMap
      Left decodeError ->
        Left $ "Error parsing JSON: " <> printJsonDecodeError decodeError
