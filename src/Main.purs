-- | module attempting to replicate functionality from https://codepen.io/chingy/pen/Exxvpjo using Halogen and Purescript
module Main where

import CSS.Property
import CSS.Selector
import CSS.Stylesheet
import CSS.Text.Transform
import Prelude
import Prelude
import Web.HTML.Common
import Data.Array (deleteAt, fold, insertAt, mapWithIndex, splitAt, (!!))
import Data.Array (deleteAt, insertAt, (!!))
import Data.Function (identity)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maybe (fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Halogen (ClassName(..), ElemName(..))
import Halogen as H
import Halogen as H
import Halogen.Aff as HA
import Halogen.Aff.Driver as H
import Halogen.HTML (HTML)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Halogen.HTML.Elements (div, element)
import Halogen.HTML.Elements as HEL
import Halogen.HTML.Events (handler)
import Halogen.HTML.Events as HEV
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT, updateStore)
import Halogen.Store.Select (selectEq, select, Selector) as Store
import Halogen.VDom.Driver (runUI)
import Halogen.VDom.Types (ElemName(..))
import Web.DOM (Element)
import Web.Event.Event (Event, EventType(..))
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent.EventTypes as DE
import Web.PointerEvent.PointerEvent (PointerEvent, fromEvent, fromMouseEvent, toMouseEvent)
import Web.UIEvent.MouseEvent (MouseEvent(..), clientX, clientY, toEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as MET

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
  { rows :: Array String
  , dragState :: DragState
  , dragOverIndex :: Maybe Int
  }

data Action
  = StartDrag Int Int Int
  | MoveDrag Int Int
  | DragOver Int
  | EndDrag
  | NoOp

initialStore :: Store
initialStore =  
  { rows: [ "April Douglas - Health Educator"
          , "Salma Mcbride - Mental Health Counselor"
          , "Kassandra Donovan - Makeup Artists"
          , "Yosef Hartman - Theatrical and Performance"
          , "Ronald Mayo - Plant Etiologist"
          , "Trey Woolley - Maxillofacial Surgeon"
          ]
  , dragState: { index: Nothing, originalX: 0, originalY: 0, currentX: 0, currentY: 0, isDragging: false }
  , dragOverIndex: Nothing
  }

component :: forall q o m. MonadAff m => H.Component q Unit o m
component = H.mkComponent
  { initialState: const initialStore
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

render :: forall m. Store -> H.ComponentHTML Action () m
render state =
  HH.div []
    [ HH.table
        [ HP.class_ (HH.ClassName "draggable-table") ]
        [ HH.thead_ [ HH.tr_ [ HH.th_ [ HH.text "Name" ], HH.th_ [ HH.text "Occupation" ]]]
        , HH.tbody_ $ mapWithIndex (\index row -> renderRow index row state.dragState.index) state.rows
        ]
    ]

renderRow :: forall m. Int -> String -> Maybe Int -> H.ComponentHTML Action () m
renderRow index row dragging =
  HH.tr
    [ HP.classes $ ClassName <$> ["draggable-table__row"] <>
        if Just index == dragging then ["is-dragging"] else []
    , onMouseDown $ \event -> 
        let
          x = MouseEvent.clientX event
          y = MouseEvent.clientY event
        in
          StartDrag index x y
    , onMouseMove $ \event ->
        let
          x = MouseEvent.clientX event
          y = MouseEvent.clientY event
        in
          MoveDrag x y
    , onMouseUp $ const EndDrag
    , onDragOver $ const $ DragOver index
    , HP.draggable true
    ]
    [ HH.td_ [ HH.text row ] ]

renderCell :: forall m. MonadAff m => Int -> String -> H.ComponentHTML Action () m
renderCell index content = 
  HH.td 
    [ HP.class_ (HH.ClassName "table-cell") ]
    [ HH.text content ]  

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM Store Action o m Unit
handleAction action = case action of
  StartDrag index x y -> H.modify_ $ \s -> s { dragState = { index: Just index, originalX: x, originalY: y, currentX: x, currentY: y, isDragging: true }}
  MoveDrag x y -> H.modify_ $ \s -> s { dragState = s.dragState { currentX = x, currentY = y }}
  DragOver index -> H.modify_ $ \s -> s { dragOverIndex = Just index }
  EndDrag -> H.modify_ $ \s -> s { dragState = s.dragState { index = Nothing, isDragging = false }, dragOverIndex = Nothing }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  void $ H.runUI component unit body

moveRow :: Int -> Int -> Array String -> Array String
moveRow from to rows =
  let
    draggedRow = fromMaybe "" $ rows !! from
    rowsWithoutDragged = fromMaybe rows $ deleteAt from rows
  in
    fromMaybe rowsWithoutDragged $ insertAt to draggedRow rowsWithoutDragged

-- Create event handlers
onMouseDown :: forall r. (MouseEvent -> Action) -> IProp (onMouseDown :: MouseEvent | r) Action
onMouseDown f = handler MET.mousedown $ \ev ->
  case fromEvent ev of
    Just pe -> f (toMouseEvent pe)
    Nothing -> NoOp

onMouseMove :: forall r. (MouseEvent -> Action) -> IProp (onMouseMove :: MouseEvent | r) Action
onMouseMove f = handler MET.mousemove $ \ev -> 
  case fromEvent ev of
    Just pe -> f (toMouseEvent pe)
    Nothing -> NoOp

onMouseUp :: forall r. (MouseEvent -> Action) -> IProp (onMouseUp :: MouseEvent | r) Action
onMouseUp f = handler MET.mouseup $ \ev ->
  case fromEvent ev of
    Just pe -> f (toMouseEvent pe)
    Nothing -> NoOp

onDragOver :: forall r. (MouseEvent -> Action) -> IProp (onDragOver :: DragEvent | r) Action
onDragOver f = handler DE.dragover $ \ev ->
  case fromEvent ev of
    Just pe -> f (toMouseEvent pe)
    Nothing -> NoOp