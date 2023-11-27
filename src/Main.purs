module Main (main) where

import Prelude (Unit, bind, discard, pure, ($), (<>), (==))
import Prelude
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Class.Console (log)
import Web.HTML (window)
import Web.DOM.Document (createElement)
import Web.HTML.HTMLDocument (HTMLDocument, body)
import Web.HTML.Window (document)
import Web.HTML.HTMLElement (HTMLElement)
import Web.DOM.ParentNode
import Web.DOM.Node (Node, appendChild)
import Web.DOM.Node (textContent)
import Web.DOM.Element (Element)
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.HTML.HTMLInputElement (fromElement, value)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Affjax (AffjaxDriver, request, defaultRequest, printError, Response)
import Affjax (AffjaxDriver, get, request, defaultRequest)
import Affjax.Web as AW
import Affjax.ResponseHeader
import Affjax.ResponseFormat (json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Decode.Class (class DecodeJson, (.=), (.?))
import Data.Argonaut.Parser (jsonParser)
import Data.Map as Map
import Form (render, form)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Foreign

foreign import _ajax :: 
  forall a.
  AffjaxDriver -> 
  String -> 
  String -> 
  (String -> String -> ResponseHeader) -> 
  AjaxRequest a -> 
  EffectFnAff (Response Foreign)

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

foreign import setHTML :: String -> Effect Unit

type ActivePlayers = { officialPlayers :: Map String Player }

instance decodeJsonActivePlayers :: DecodeJson ActivePlayers where
  decodeJson json = do
    obj <- toObject json
    players <- obj .? "officialPlayers"
    pure { officialPlayers: players }

instance decodeJsonPlayer :: DecodeJson Player where
  decodeJson json = do
    obj <- toObject json
    active <- obj .? "active"
    batSide <- obj .? "batSide"
    currentTeam <- obj .? "currentTeam"
    nameSlug <- obj .? "nameSlug"
    pitchHand <- obj .? "pitchHand"
    primaryPosition <- obj .? "primaryPosition"
    useLastName <- obj .? "useLastName"
    useName <- obj .? "useName"
    pure $ Player active batSide currentTeam nameSlug pitchHand primaryPosition useLastName useName

instance decodeJsonPlayers :: DecodeJson (Map String Player) where
  decodeJson json = do
    obj <- toObject json
    traverse decodeJson (toMap obj)

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
          addEventListener (EventType "submit") eventHandler false el  -- Use el directly
        Nothing -> log "Form element not found"
    Nothing -> log "Body element not found"


handleSubmit :: HTMLDocument -> Event -> Effect Unit
handleSubmit htmlDoc event = do
  preventDefault event
  bodyEl <- body htmlDoc
  case bodyEl of
    Just el -> do
      positionInputEl <- querySelector (QuerySelector "#position") el  -- Use el directly
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
    Just inputEl -> Just <$> value inputEl
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
        Right activePlayers -> 
          pure $ Map.filter (\player -> player.primaryPosition == position) (activePlayers.officialPlayers)
        Left decodeError -> do
          log $ "Error parsing JSON: " <> printJsonDecodeError decodeError
          pure Map.empty



