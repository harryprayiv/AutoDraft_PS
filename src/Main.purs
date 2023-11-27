module Main (main) where

import Prelude (Unit, bind, discard, pure, ($), (<>), (==))
import Prelude
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
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
import Affjax (get)
import Affjax (get)
import Affjax.ResponseFormat (json)  -- Make sure this is imported correctly
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Map as Map
import Form (render, form)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (traverse_)



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
  response <- get json "./appData/rosters/activePlayers.json"  -- Use json correctly here
  case response of
    Left error -> do
      log $ "Error loading JSON: " <> show error
      pure Map.empty
    Right res -> case decodeJson res.body of
      Right activePlayers -> 
        pure $ Map.filter (\player -> player.primaryPosition == position) (activePlayers.officialPlayers)
      Left decodeError -> do
        log $ "Error parsing JSON: " <> printJsonDecodeError decodeError
        pure Map.empty
