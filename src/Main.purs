module Main (main) where

-- import Affjax.ResponseHeader
-- import Affjax (AffjaxDriver, get, request, defaultRequest)
-- import Affjax (AffjaxDriver, request, defaultRequest, printError, Response)
-- import Affjax.ResponseFormat (json)
-- import Affjax.Web as AW
-- import Data.Either (Either(..))
-- import Data.Foldable (foldl, traverse_)
-- import Data.FoldableWithIndex
-- import Data.Tuple (Tuple(..))
-- import Data.Map (Map)
-- import Data.Map as Map
-- import Data.Maybe (Maybe(..), maybe)
-- import Data.Set (toMap)
-- import Data.Traversable (sequence)
-- import Data.Traversable (traverse)
-- import Effect.Aff (Aff)
-- import Effect.Aff.Compat (EffectFnAff)
-- import Effect.Class.Console (log)
-- import Prelude (Unit, bind, discard, pure, ($), (<>), (==))
-- import Web.DOM.Document (createElement, createTextNode, toNonElementParentNode)
-- import Web.DOM.Element (Element)
-- import Web.DOM.Node (Node, appendChild)
-- import Web.DOM.Node (textContent)
-- import Web.DOM.ParentNode (QuerySelector(..), querySelector)
-- import Web.Event.Event (Event, EventType(..), preventDefault)
-- import Web.Event.EventTarget (eventListener, addEventListener)
-- import Web.HTML.HTMLDocument (HTMLDocument, body)
-- import Web.HTML.HTMLElement (HTMLElement)
-- import Web.HTML.HTMLInputElement (HTMLInputElement, value, fromElement)
-- import Web.HTML.HTMLInputElement as HTMLInputElement
import Foreign
import Foreign.Object (Object, lookup)
import Foreign.Object as Object
import Prelude
import Web.DOM.ParentNode
import Effect (Effect)
import Form (render, form)
import Player (setupEventListeners)
import Web.HTML (window)
import Web.HTML.Window (document)

foreign import setHTML :: String -> Effect Unit

main :: Effect Unit
main = do
  setHTML $ render form
  win <- window
  doc <- document win
  setupEventListeners doc
