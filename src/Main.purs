module Main (main) where

import Affjax.ResponseHeader
import Data.FoldableWithIndex
import Foreign
import Prelude
import Web.DOM.ParentNode

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Class.Console (log)

import Affjax (AffjaxDriver, get, request, defaultRequest)
import Affjax (AffjaxDriver, request, defaultRequest, printError, Response)
import Affjax (defaultRequest, request, Response)
import Affjax.ResponseFormat (json)
import Affjax.ResponseFormat (json)
import Affjax.Web as AW
import Affjax.Web as AW
import Data.Argonaut (Json, jsonEmptyObject, stringify)
import Data.Argonaut.Core (toObject)
import Data.Argonaut.Core (toObject, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Foldable (foldl, traverse_)
import Data.Map (Map, toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (toMap)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM as HM
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, discard, pure, ($), (<>), (==))
import Web.DOM.Document (createElement, createTextNode, toNonElementParentNode)
import Web.DOM.Element (Element)
import Web.DOM.Node (Node, appendChild, textContent)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, body)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLInputElement (HTMLInputElement, value, fromElement)
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window (document)

import Form (render, form)

import Foreign.Object (Object, lookup)
import Foreign.Object as Object

foreign import _ajax
  :: forall a
   . AffjaxDriver
  -> String
  -> String
  -> (String -> String -> ResponseHeader)
  -> AjaxRequest a
  -> EffectFnAff (Response Foreign)

type AjaxRequest :: forall k. k -> Type
type AjaxRequest a =
  { method :: String
  , url :: String
  , headers :: Array { field :: String, value :: String }
  , content :: Maybe Foreign
  , responseType :: String
  , username :: Maybe String
  , password :: Maybe String
  , withCredentials :: Boolean
  , timeout :: Int
  }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action
  = HandleInput String
  | Submit
  | LoadData
  | SetPlayers (Map String Player)

type State =
  { players :: Map String Player
  , positionInput :: String
  }

-- Halogen Component
component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const { players: Map.empty, positionInput: "" }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just LoadData
        }
    }
  where
  render state =
    HH.div_
      [ HH.input [ HE.onValueInput (Just <<< HandleInput) ]
      , HH.button [ HE.onClick (const Submit) ] [ HH.text "Submit" ]
      , HH.div_ (renderPlayers state.players)
      ]

  handleAction = case _ of
    HandleInput input -> H.modify_ _ { positionInput = input }
    Submit -> filterPlayers
    LoadData -> loadData
    SetPlayers players -> H.modify_ _ { players = players }

  filterPlayers = do
    state <- H.get
    let filtered = Map.filter (\p -> p.primaryPosition == state.positionInput) state.players
    H.modify_ _ { players = filtered }

  loadData = do
    players <- loadPlayers
    H.modify_ _ { players = players }

  renderPlayers players =
    map toUnfoldable players >>= \player ->
      [ HH.div_
          [ HH.text (player.useName <> " - Position: " <> player.primaryPosition) ]
      ]

type PlayerWithKey =
  { key :: String
  , player :: Player
  }

render :: State -> H.ComponentHTML Action
render state =
  HH.div_
    [ HH.form_ [ HE.onSubmit (const (Just Submit)) ]
        [ HH.input [ HP.type_ HP.InputText, HE.onValueInput HandleInput, HP.value state.positionInput ]
        , HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "Submit" ]
        ]
    , HH.div_ (renderPlayers state.players)
    ]

renderPlayers :: Map String Player -> Array (H.ComponentHTML action)
renderPlayers players =
  map (\(Tuple _ player) -> HH.div_ [ HH.text $ player.useName <> " - Position: " <> player.primaryPosition ])
    (toUnfoldable players)

renderPlayer :: { key :: String, player :: Player } -> H.ComponentHTML action
renderPlayer playerWithKey =
  HH.div_ [ HH.text $ playerWithKey.player.useName <> " - Position: " <> playerWithKey.player.primaryPosition ]

eval :: Action -> HM.HalogenM State Action () Void m Unit
eval action = case action of
  HandleInput input -> H.modify_ (_ { positionInput = input })
  Submit -> filterPlayers
  LoadData -> do
    players <- H.liftAff loadPlayers
    H.modify_ (_ { players = players })
  SetPlayers players -> H.modify_ (_ { players = players })
  where
  filterPlayers = do
    state <- H.get
    let filtered = Map.filter (\p -> p.primaryPosition == state.positionInput) state.players
    H.modify_ (_ { players = filtered })

-- Function to load and decode the player data
loadPlayers :: Aff (Map String Player)
loadPlayers = do
  let req = defaultRequest { url = "./appData/rosters/activePlayers.json", responseFormat = json }
  response <- AW.request req
  case response of
    Left error -> do
      log $ "Error loading JSON: " <> printError error
      pure Map.empty
    Right res ->
      case decodeJson res.body of
        Right (ActivePlayers playersMapWrapped) ->
          pure $ unwrapPlayersMap playersMapWrapped
        Left decodeError -> do
          log $ "Error parsing JSON: " <> printJsonDecodeError decodeError
          pure Map.empty

newtype PlayersMap = PlayersMap (Map String Player)

type PlayerEntry = { key :: String, playerJson :: Json }

type Player =
  { active :: Boolean
  , batSide :: String
  , currentTeam :: Int
  , nameSlug :: String
  , pitchHand :: String
  , playerId :: Int
  , primaryPosition :: String
  , useLastName :: String
  , useName :: String
  }

type Players = Map.Map String Player

newtype ActivePlayers = ActivePlayers PlayersMap

instance decodeJsonActivePlayers :: DecodeJson ActivePlayers where
  decodeJson json = do
    obj <- case toObject json of
      Just o -> pure o
      Nothing -> Left $ TypeMismatch "Expected an object"
    playersObj <- obj .: "officialPlayers"
    playersMapWrapped <- decodeJson playersObj
    pure $ ActivePlayers playersMapWrapped

instance decodeJsonPlayersMap :: DecodeJson PlayersMap where
  decodeJson json = do
    obj <- case toObject json of
      Just o -> pure o
      Nothing -> Left $ TypeMismatch "Expected an object"
    playersObj <- obj .:? "officialPlayers" >>= maybe (pure jsonEmptyObject) pure

    case toObject playersObj of
      Just players -> do
        let
          entries :: Array (Tuple String Json)
          entries = Object.toUnfoldable players

        let
          buildMap acc (Tuple key playerJson) =
            case decodeJsonPlayer playerJson of
              Right player -> Map.insert key player acc
              Left _ -> acc

        pure $ PlayersMap $ foldl buildMap Map.empty entries
      Nothing -> Left $ TypeMismatch "Expected 'officialPlayers' to be an object"

unwrapPlayersMap :: PlayersMap -> Map String Player
unwrapPlayersMap (PlayersMap map) = map

decodeJsonPlayer :: Json -> Either JsonDecodeError Player
decodeJsonPlayer json = do
  obj <- case toObject json of
    Just o -> Right o
    Nothing -> Left $ TypeMismatch "Invalid JSON structure for Player"

  -- Decode each field with proper error handling
  active <- decodeField obj "active"
  batSide <- decodeField obj "batSide"
  currentTeam <- decodeField obj "currentTeam"
  nameSlug <- decodeField obj "nameSlug"
  pitchHand <- decodeField obj "pitchHand"
  playerId <- decodeField obj "playerId"
  primaryPosition <- decodeField obj "primaryPosition"
  useLastName <- decodeField obj "useLastName"
  useName <- decodeField obj "useName"

  -- Return a record
  pure
    { active: active
    , batSide: batSide
    , currentTeam: currentTeam
    , nameSlug: nameSlug
    , pitchHand: pitchHand
    , playerId: playerId
    , primaryPosition: primaryPosition
    , useLastName: useLastName
    , useName: useName
    }

decodeField :: forall a. DecodeJson a => Object Json -> String -> Either JsonDecodeError a
decodeField obj fieldName = case lookup fieldName obj of
  Just value -> decodeJson value
  Nothing -> Left MissingValue
