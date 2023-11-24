module Main (main) where

import Prelude
import Effect (Effect)
import Form (render, form)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (jsonEmptyObject, stringify)
import Data.Argonaut.Decode.Class (DecodeJson)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.Event.Event (defaultPrevented, preventDefault)
import Web.Event.EventTarget (addEventListener)
-- import Web.Event.Event (Event)
import Web.HTML.HTMLInputElement (fromElement, value)
-- import Web.HTML.Element (Element)
import Web.HTML.HTMLDocument (querySelector)

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

-- Load JSON, filter players, and display results
-- You'll need to write the logic to load JSON and filter players

getInputValue :: Maybe Element -> Effect String
getInputValue (Just el) = fromElement el >>= maybe (pure "") value
getInputValue Nothing = pure ""
