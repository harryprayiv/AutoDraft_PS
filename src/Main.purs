-- | module attempting to replicate functionality from https://codepen.io/chingy/pen/Exxvpjo using Halogen and Purescript
module Main where

import Prelude
import Web.DOM

import Data.Array (deleteAt, insertAt, mapWithIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, modify_) as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events (onDragOver, onMouseDown, onMouseMove, onMouseUp)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected)
import Halogen.Store.Select as Store
import Halogen.VDom.Driver (runUI)
import Web.DOM.Element (Element)
import Web.DOM.Element (getBoundingClientRect, DOMRect)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML.Common (ClassName(..))
import Web.HTML.Event.DragEvent.EventTypes (dragover)
import Web.HTML.Event.DragEvent.EventTypes as MDE
import Web.UIEvent.MouseEvent as MEV
import Web.UIEvent.MouseEvent.EventTypes (mousedown, mousemove, mouseup)
import Web.UIEvent.MouseEvent.EventTypes as MVT

type Input = Unit

deriveState :: Connected Store Input -> Store
deriveState { context } = context

selectStore :: Store.Selector Store Store
selectStore = Store.selectEq \store -> store

type DragState = 
  { index :: Maybe Int
  , originalX :: Int
  , originalY :: Int
  , currentX :: Int
  , currentY :: Int
  , isDragging :: Boolean
  }

type Store = 
  { rows :: Array (Tuple String String)
  , dragState :: DragState
  , dragOverIndex :: Maybe Int
  }

data Action
  = StartDrag Int Int Int Int
  | MoveDrag Int Int
  | DragOver Int Int 
  | EndDrag
  | NoOp

initialStore :: Store
initialStore =  
  { rows: [ Tuple "April Douglas" "Health Educator"
          , Tuple "Salma Mcbride" "Mental Health Counselor"
          , Tuple "Kassandra Donovan" "Makeup Artists"
          , Tuple "Yosef Hartman" "Theatrical and Performance"
          , Tuple "Ronald Mayo" "Plant Etiologist"
          , Tuple "Trey Woolley" "Maxillofacial Surgeon"
          ]
  , dragState: { index: Nothing, originalX: 0, originalY: 0, currentX: 0, currentY: 0, isDragging: false }
  , dragOverIndex: Nothing
  }

component :: forall q m. MonadAff m => H.Component q Unit Void m
component = 
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = \_ -> pure Nothing
        , receive = const Nothing
        , initialize = Nothing
        , finalize = Nothing
        }
    }

initialState :: Input -> Store
initialState _ = initialStore

-- starting with a uniform row height for simplicity
rowHeight :: Int
rowHeight = 50  -- Example height, adjust based on your UI

-- Calculate drop index based on currentY position of the drag and uniform row height
findDropIndex :: Int -> Maybe Int
findDropIndex currentY =
  let
    rowIndex = currentY `div` rowHeight
  in
    Just rowIndex

render :: forall m. MonadAff m => Store -> H.ComponentHTML Action () m
render state =
  HH.div []
    [ HH.table
        [ HP.class_ (HH.ClassName "draggable-table") ]
        [ HH.thead_ [ HH.tr_ [ HH.th_ [ HH.text "Name" ], HH.th_ [ HH.text "Occupation" ]]]
        , HH.tbody_ $ mapWithIndex (\index (Tuple name occupation) -> renderRow index name occupation state.dragState.index) state.rows
        ]
    ]

renderRow :: forall m. Int -> String -> String -> Maybe Int -> H.ComponentHTML Action () m
renderRow index name occupation dragging =
  HH.tr
    [ HP.classes $ ClassName <$> ["draggable-table__row"] <>
        if Just index == dragging then ["is-dragging"] else []
    , onMouseDown $ HE.handler MVT.mousedown (\event -> StartDrag index (MEV.clientX event) (MEV.clientY event))
    , onMouseMove $ HE.handler MVT.mousemove (\event -> MoveDrag (MEV.clientX event) (MEV.clientY event))
    , onMouseUp $ HE.handler MVT.mouseup (\_ -> EndDrag)
    , onDragOver $ HE.handler MDE.dragover (\event -> DragOver index (MEV.clientY event))
    , HP.draggable true
    ]
    [ renderCell name, renderCell occupation ]

renderCell :: forall m. String -> H.ComponentHTML Action () m
renderCell content =
  HH.td 
    [ HP.class_ (HH.ClassName "table-cell") ]
    [ HH.text content ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM Store Action () Void m Unit
handleAction action =
  case action of
    StartDrag index x y ->
      H.modify_ \s -> s { dragState = { index: Just index, originalX: x, originalY: y, currentX: x, currentY: y, isDragging: true }}
    MoveDrag x y ->
      H.modify_ \s -> s { dragState = s.dragState { currentX = x, currentY = y }}
    DragOver mouseY -> 
      H.modify_ \s -> s { dragOverIndex = findDropIndex mouseY }
    EndDrag ->
      H.modify_ \s -> case s.dragState.index of
        Just fromIndex -> case s.dragOverIndex of
          Just toIndex -> s { rows = moveRow fromIndex toIndex s.rows
                            , dragState = resetDragState
                            , dragOverIndex = Nothing }
          Nothing -> s { dragState = resetDragState, dragOverIndex = Nothing }
        Nothing -> s
    NoOp -> pure unit

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body 

moveRow :: Int -> Int -> Array (Tuple String String) -> Array (Tuple String String)
moveRow from to rows =
  let
    draggedRow = fromMaybe (Tuple "" "") $ rows !! from
    rowsWithoutDragged = fromMaybe rows $ deleteAt from rows
  in
    fromMaybe rowsWithoutDragged $ insertAt to draggedRow rowsWithoutDragged

-- Function to reset the drag state
resetDragState :: DragState
resetDragState = 
  { index: Nothing, originalX: 0, originalY: 0, currentX: 0, currentY: 0, isDragging: false }

-- This function is conceptual and might not directly compile without proper imports and adjustments
getElementSizeAndPosition :: String -> Effect (Maybe DOMRect)
getElementSizeAndPosition elementId = do
  document <- HA.selectElement
  maybeElement <- getElementById elementId document
  case maybeElement of
    Just element -> do
      rect <- getBoundingClientRect element
      pure $ Just rect
    Nothing -> pure Nothing  