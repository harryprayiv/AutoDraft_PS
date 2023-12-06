module MyComponent
  ( component
  ) where

import Player
import Prelude

import Affjax (AffjaxDriver, request, defaultRequest, printError, Response)
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
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object, lookup)
import Foreign.Object as Object
import Halogen (HalogenM, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = GetState (State -> a) -- Define this based on your needs

type State = 
  { players :: Map String Player
  , positionInput :: String
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = HandleInput String
  | Submit
  | SetPlayers (Map String Player)
  | HandleError String

component :: forall m. MonadAff m => H.Component Query State Action m
component = H.mkComponent
  { initialState: \_ -> initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
    initialState :: State
    initialState = 
      { players: Map.empty
      , positionInput: ""
      , loading: false
      , error: Nothing
      }

    render :: State -> H.ComponentHTML Action Void m
    render state = 
      HH.div_
        [ inputField state.positionInput,
          submitButton state.loading,
          maybe HH.div_ errorDiv state.error,
          playersTable state.players
        ]
        
    handleAction :: forall output. Action -> H.HalogenM State Action () m output
    handleAction action = case action of
      HandleInput input -> H.modify_ (_ { positionInput = input })
      Submit -> do
        currentState <- H.get
        _ <- H.liftAff $ loadData currentState.positionInput
        pure unit
      SetPlayers players -> H.modify_ (_ { players = players, loading = false })
      HandleError errorMsg -> H.modify_ (_ { error = Just errorMsg, loading = false })

    inputField :: String -> H.ComponentHTML Action () m
    inputField inputValue = HH.input [ HP.type_ HP.InputText, HP.value inputValue, HE.onValueInput HandleInput ]

    submitButton :: Boolean -> H.ComponentHTML Action () m
    submitButton loading = 
      HH.button 
        [ HP.disabled loading, HE.onClick $ const Submit ] 
        [ HH.text (if loading then "Loading..." else "Load Players") ]

    playersTable :: Map String Player -> H.ComponentHTML Action () m
    playersTable players = HH.div_ $ map renderPlayer $ Map.toUnfoldable players

    renderPlayer :: Tuple String Player -> H.ComponentHTML Action () m
    renderPlayer (Tuple _ player) = HH.div_ [ HH.text $ player.useName <> " - Position: " <> player.primaryPosition ]

    errorDiv :: String -> H.ComponentHTML Action () m
    errorDiv errorMsg = HH.div_ [ HH.text errorMsg ]

    loadData :: String -> Aff Unit
    loadData positionInput = do
      _ <- H.liftEffect $ H.modify_ (_ { loading = true })
      response <- fetchPlayers
      case response of
        Right players -> do
          let filteredPlayers = Map.filter (\p -> show p.primaryPosition == positionInput) players
          _ <- H.liftEffect $ H.modify_ (\s -> s { players = filteredPlayers, loading = false })
          pure unit
        Left errorMsg -> do
          _ <- H.liftEffect $ H.modify_ (_ { error = Just errorMsg, loading = false })
          pure unit

    fetchPlayers :: Aff (Either String (Map String Player))
    fetchPlayers = do
      let req = defaultRequest 
                { url = "./appData/rosters/activePlayers.json"
                , responseFormat = json
                }
      response <- AW.request req
      pure $ case response of
        Left err -> Left $ "Error loading JSON: " <> printError err
        Right res -> case decodeJson res.body of
          Right (ActivePlayers (PlayersMap playersMap)) -> Right playersMap
          Left decodeError -> Left $ "Error parsing JSON: " <> printJsonDecodeError decodeError