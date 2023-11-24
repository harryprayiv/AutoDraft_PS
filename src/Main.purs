module Main (main) where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Aff (launchAff_)
import Effect.Aff
import Data.Argonaut.Decode (decodeJson, (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Core (jsonEmptyObject, stringify)
import Data.Argonaut.Parser (jsonParser)
import Effect.Class.Console (log)
import Web.HTML (window)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.Event.Event (defaultPrevented, preventDefault)
import Web.Event.EventTarget (addEventListener)
import Web.Event.Internal.Types (Event)
-- import Web.Event.Internal.Types (QuerySelector(..))
import Web.DOM.Element (Element)
import Web.DOM.Element (toParentNode)
import Web.HTML.HTMLInputElement (fromElement, value)
import Web.DOM.Document
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Affjax.Web (get)
import Affjax (printError)
import Affjax.ResponseFormat (json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Form (render, form)
import Data.Map.Internal 
import Web.HTML.HTMLDocument (fromDocument, body)

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

type Players = Map String Player

type ActivePlayers =
  { officialPlayers :: Players
  }

main :: Effect Unit
main = do
  setHTML $ render form
  win <- window
  doc <- document win
  setupEventListeners doc

setupEventListeners :: Document -> Effect Unit
setupEventListeners doc = do
  Just formEl <- querySelector "form" (toDocument doc)
  addEventListener "submit" (handleSubmit doc) false formEl

handleSubmit :: Document -> Event -> Effect Unit
handleSubmit doc event = do
  preventDefault event
  case fromDocument doc of
    Just htmlDoc -> do
      Just bodyEl <- body htmlDoc
      let parentNode = toParentNode (toElement bodyEl)
      Just positionInputEl <- querySelector (QuerySelector "#position") parentNode
      positionValue <- getInputValue positionInputEl
      launchAff_ $ loadAndFilterPlayers positionValue
    Nothing -> log "Document is not an HTMLDocument"

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
        -- Log or process filteredPlayers
        liftEffect $ logShow filteredPlayers
      Left decodeError ->
        log $ "Error parsing JSON: " <> printJsonDecodeError decodeError

genericErrorToString :: forall e. Show e => e -> String
genericErrorToString = show

