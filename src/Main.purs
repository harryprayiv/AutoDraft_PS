module Main (main) where

import Affjax.ResponseHeader
import Foreign
import Foreign.Object (Object, lookup)
import Prelude
import Web.DOM.ParentNode

import Affjax (AffjaxDriver, get, request, defaultRequest)
import Affjax (AffjaxDriver, request, defaultRequest, printError, Response)
import Affjax.ResponseFormat (json)
import Affjax.Web as AW
import Data.Argonaut (Json, jsonEmptyObject, stringify)
import Data.Argonaut.Core (toObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (toMap)
import Data.Traversable (sequence)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Class.Console (log)
import Form (render, form)
import Prelude (Unit, bind, discard, pure, ($), (<>), (==))
import Web.DOM.Document (createElement)
import Web.DOM.Element (Element)
import Web.DOM.Node (Node, appendChild)
import Web.DOM.Node (textContent)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, body)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLInputElement (HTMLInputElement, value, fromElement)
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window (document)

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
  , responseType :: String -- This should be a String, not Json
  , username :: Maybe String
  , password :: Maybe String
  , withCredentials :: Boolean
  , timeout :: Int
  }

foreign import setHTML :: String -> Effect Unit

newtype ActivePlayers = ActivePlayers PlayersMap

instance decodeJsonActivePlayers :: DecodeJson ActivePlayers where
  decodeJson json = do
    obj <- jsonParser json
    playersObj <- obj .: "officialPlayers"
    playersMapWrapped <- decodeJson playersObj -- Assuming playersObj is a JSON object
    pure $ ActivePlayers playersMapWrapped

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

  pure $ Player
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

newtype PlayersMap = PlayersMap (Map String Player)

instance decodeJsonPlayersMap :: DecodeJson PlayersMap where
  decodeJson json = do
    obj <- jsonParser json
    playerObjs <- obj .:? "officialPlayers" >>= maybe (pure jsonEmptyObject) pure
    players <- traverse decodeJsonPlayer playerObjs -- Changed from decodePlayer to decodeJsonPlayer
    case players of
      -- Right ps -> pure $ PlayersMap $ Map.fromList $ map (\player -> (show $ playerId player, player)) ps
      Left error -> pure $ Left error

newtype Player = Player
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

main :: Effect Unit
main = do
  setHTML $ render form
  win <- window
  doc <- document win
  setupEventListeners doc

setupEventListeners :: HTMLDocument -> Effect Unit
setupEventListeners htmlDoc = do
  bodyEl <- body htmlDoc
  case bodyEl of
    Just bodyElement -> do
      formEl <- querySelector (QuerySelector "form") bodyElement
      case formEl of
        Just el -> do
          eventHandler <- eventListener (handleSubmit htmlDoc)
          addEventListener (EventType "submit") eventHandler false el -- Use el directly
        Nothing -> log "Form element not found"
    Nothing -> log "Body element not found"

handleSubmit :: HTMLDocument -> Event -> Effect Unit
handleSubmit htmlDoc event = do
  preventDefault event
  bodyEl <- body htmlDoc
  case bodyEl of
    Just el -> do
      positionInputEl <- querySelector (QuerySelector "#position") el -- Use el directly
      positionValue <- getInputValue positionInputEl
      case positionValue of
        Just pos -> loadAndFilterPlayers pos >>= displayPlayers htmlDoc
        Nothing -> log "Position input element not found"
    Nothing -> log "Body element not found"

displayPlayers :: HTMLDocument -> Players -> Effect Unit
displayPlayers htmlDoc players = do
  maybeBody <- body htmlDoc
  case maybeBody of
    Just bodyElement -> do
      let playerValues = Map.values players
      playerElements <- traverse (createPlayerElement htmlDoc) playerValues
      traverse_ (appendChild bodyElement) playerElements
    Nothing -> pure unit

createPlayerElement :: HTMLDocument -> Player -> Effect Element
createPlayerElement htmlDoc player = do
  playerDiv <- createElement htmlDoc "div"
  _ <- textContent (player.useName <> " - Position: " <> player.primaryPosition) playerDiv
  pure playerDiv

getInputValue :: Maybe Element -> Effect (Maybe String)
getInputValue maybeEl = case maybeEl of
  Just el -> case fromElement el of
    Just inputEl -> Just <$> HTMLInputElement.value inputEl
    Nothing -> pure Nothing
  Nothing -> pure Nothing

loadAndFilterPlayers :: String -> Aff Players
loadAndFilterPlayers position = do
  let req = defaultRequest { url = "./appData/rosters/activePlayers.json", responseFormat = json }
  response <- request AW.driver req
  case response of
    Left error -> do
      log $ "Error loading JSON: " <> printError error
      pure Map.empty
    Right res ->
      case decodeJson res.body of
        Right (ActivePlayers playersMapWrapped) ->
          let
            playersMap = unwrapPlayersMap playersMapWrapped

            filterFunc :: String -> Player -> Boolean
            filterFunc _ (Player player) = player.primaryPosition == position
          in
            pure $ Map.filterWithKey filterFunc playersMap
        Left decodeError -> do
          log $ "Error parsing JSON: " <> printJsonDecodeError decodeError
          pure Map.empty