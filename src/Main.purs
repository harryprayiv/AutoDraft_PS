module Main (main) where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Data.Argonaut.Decode (decodeJson, (.:?))
import Data.Argonaut.Core (jsonEmptyObject, stringify)
import Data.Argonaut.Parser (jsonParser)
import Effect.Class.Console (log)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.Event.Event (defaultPrevented, preventDefault)
import Web.Event.EventTarget (addEventListener)
import Web.Event.Internal.Types (Event)
import Web.DOM.Element (Element)
import Web.HTML.HTMLInputElement (fromElement, value)
import Web.DOM.Document
import Web.DOM.ParentNode (querySelector)
import Affjax.Web (get)
-- import Web.HTML.Window
-- import Network.HTTP.Affjax.Web (Response)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing), maybe)

import Form (render, form)

foreign import setHTML :: String -> Effect Unit

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
  Just positionInputEl <- querySelector "#position" (toDocument doc)
  positionValue <- getInputValue positionInputEl
  launchAff_ $ loadAndFilterPlayers positionValue

getInputValue :: Maybe Element -> Effect String
getInputValue (Just el) = fromElement el >>= maybe (pure "") value
getInputValue Nothing = pure ""

loadAndFilterPlayers :: String -> Effect Unit
loadAndFilterPlayers position = do
  response <- get "./appData/rosters/activePlayers.json"
  case response of
    Left error -> log $ "Error loading JSON: " <> show error
    Right res -> case decodeJson res.body of
      Right json -> do
        -- Process the JSON and filter the players
        -- Implement your filtering logic here
        pure unit
      Left error -> log $ "Error parsing JSON: " <> show error