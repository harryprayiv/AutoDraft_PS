module MyComponent
  ( component
  ) where

import Prelude
import Player

import Effect.Aff (Aff)

import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Affjax.ResponseFormat (json)
import Affjax (AffjaxDriver, request, defaultRequest, printError, Response)
import Affjax.Web as AW

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Core (Json, jsonEmptyObject, toObject, jsonNull)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))

import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

import Foreign.Object (Object, lookup)
import Foreign.Object as Object

data Query a = GetState (State -> a)

type State =
  { players :: Map String Player
  , positionInput :: String
  , loading :: Boolean
  , error :: Maybe String
  }

data Message
  = PlayersLoaded (Map String Player)
  | LoadingError String

data Action
  = HandleInput String
  | Submit
  | SetPlayers (Map String Player)
  | HandleError String

component :: forall m. MonadAff m => H.Component HH.HTML () State Message m
component = H.mkComponent
  { initialState: \_ -> { players: Map.empty, positionInput: "", loading: false, error: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }
  where
  render :: State -> H.ComponentHTML Action ()
  render state =
    HH.div_
      [ inputField state.positionInput
      , submitButton state.loading
      , maybe HH.div_ errorDiv state.error
      , playersTable state.players
      ]

  eval :: forall m. MonadAff m => Action -> H.HalogenM State () Message m Unit
  eval action = case action of
    HandleInput input -> H.modify_ (_ { positionInput = input })
    Submit -> do
      currentState <- H.get
      loadData currentState.positionInput
    SetPlayers players -> H.modify_ (_ { players = players, loading = false })
    HandleError errorMsg -> H.modify_ (_ { error = Just errorMsg, loading = false })

  inputField inputValue =
    HH.input [ HP.type_ HP.InputText, HP.value inputValue, HE.onValueInput HandleInput ]

  submitButton loading =
    HH.button [ HP.disabled loading, HE.onClick $ const Submit ]
      [ HH.text (if loading then "Loading..." else "Load Players") ]

  playersTable players =
    HH.div_ $ map renderPlayer $ Map.toUnfoldable players

  renderPlayer (Tuple _ player) =
    HH.div_ [ HH.text $ player.useName <> " - Position: " <> player.primaryPosition ]

  errorDiv errorMsg = HH.div_ [ HH.text errorMsg ]

  handleAction :: forall m. MonadAff m => Action -> H.HalogenM State () Message m Unit
  handleAction action = case action of
    HandleInput input -> H.modify_ (_ { positionInput = input })
    Submit -> do
      currentState <- H.get
      loadData currentState.positionInput
    SetPlayers players -> H.modify_ (_ { players = players, loading = false })
    HandleError errorMsg -> H.modify_ (_ { error = Just errorMsg, loading = false })

  handleQuery :: Query ~> H.HalogenM State Action q Message m
  handleQuery = case _ of
    GetState k -> do
      state <- H.get
      pure $ Just $ k state

  loadData :: forall m. MonadAff m => String -> H.HalogenM State () Message m Unit
  loadData positionInput = do
    H.modify_ (_ { loading = true })
    response <- H.liftAff $ fetchPlayers
    case response of
      Right players -> do
        let filteredPlayers = Map.filter (\p -> p.primaryPosition == positionInput) players
        H.raise $ PlayersLoaded filteredPlayers
      Left errorMsg -> H.raise $ LoadingError errorMsg

fetchPlayers :: forall m. MonadAff m => m (Either String (Map String Player))
fetchPlayers = do
  let req = defaultRequest { url = "./appData/rosters/activePlayers.json", responseFormat = json }
  response <- H.liftAff $ AW.request req -- Use liftAff to lift the Aff action into the m monad
  pure $ case response of
    Left error ->
      Left $ "Error loading JSON: " <> printError error
    Right res ->
      case decodeJson res.body of
        Right (ActivePlayers (PlayersMap playersMap)) ->
          Right playersMap
        Left decodeError ->
          Left $ "Error parsing JSON: " <> printJsonDecodeError decodeError

