module Main (main) where

import Prelude (Unit, bind, discard, pure, ($), (<>), (==))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Aff
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Web.HTML (window)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (HTMLDocument, body)
import Web.Event.Event (EventType(..), preventDefault)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.Event.Internal.Types (Event)
import Web.DOM.Element (Element, toParentNode, toEventTarget)
import Web.HTML.HTMLInputElement (fromElement, value)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Affjax.Web (get)
import Affjax (printError)
import Affjax.ResponseFormat (json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Nothing, Just))
import Form (render, form)
import Data.Map as Map

foreign import setHTML :: String -> Effect Unit

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

type ActivePlayers = { officialPlayers :: Players }

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
      let parentNode = toParentNode (toElement bodyElement)
      formEl <- querySelector (QuerySelector "form") parentNode
      case formEl of
        Just el -> do
          eventHandler <- eventListener (handleSubmit htmlDoc)
          let eventTarget = toEventTarget el
          addEventListener (EventType "submit") eventHandler false eventTarget
        Nothing -> log "Form element not found"
    Nothing -> log "Body element not found"

handleSubmit :: HTMLDocument -> Event -> Effect Unit
handleSubmit htmlDoc event = do
  preventDefault event
  bodyEl <- body htmlDoc
  case bodyEl of
    Just el -> do
      let parentNode = toParentNode (toElement el)
      positionInputEl <- querySelector (QuerySelector "#position") parentNode
      case positionInputEl of
        Just inputEl -> do
          positionValue <- getInputValue (Just inputEl)
          launchAff_ $ loadAndFilterPlayers positionValue
        Nothing -> log "Position input element not found"
    Nothing -> log "Body element not found"

getInputValue :: Maybe Element -> Effect String
getInputValue maybeEl = case maybeEl of
  Just el -> do
    case fromElement el of
      Just inputEl -> value inputEl
      Nothing -> pure ""
  Nothing -> pure ""

loadAndFilterPlayers :: String -> Aff Unit
loadAndFilterPlayers position = do
  response <- get json "./appData/rosters/activePlayers.json"
  case response of
    Left error ->
      log $ "Error loading JSON: " <> printError error
    Right res -> case decodeJson res.body :: Either JsonDecodeError ActivePlayers of
      Right activePlayers -> do
        let players = activePlayers.officialPlayers
        let filteredPlayers = Map.filter (\player -> player.primaryPosition == position) players
        liftEffect $ logShow filteredPlayers
      Left decodeError ->
        log $ "Error parsing JSON: " <> printJsonDecodeError decodeError

