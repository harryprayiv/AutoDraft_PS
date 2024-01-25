-- | module attempting to replicate functionality from https://codepen.io/chingy/pen/Exxvpjo using Halogen and Purescript
module Main where

import CSS.Property
import CSS.Selector
import CSS.Stylesheet
import CSS.Text.Transform
import Effect.Aff.Class
import Prelude
import Web.HTML.Common

import Data.Array (deleteAt, fold, insertAt, mapWithIndex, splitAt, (!!))
import Data.Function (identity)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Void (absurd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Halogen (ClassName(..), ElemName(..), HalogenM, HalogenQ, HalogenIO)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Aff.Driver as H
import Halogen.Aff.Driver as HD
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Halogen.HTML.Elements (div, element)
import Halogen.HTML.Elements as HHE
import Halogen.HTML.Events (handler, onMouseDown, onMouseUp, onDragOver)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected)
import Halogen.Store.Select as Store
import Halogen.VDom.Driver (runUI)
import Halogen.VDom.Types (ElemName(..))
import Web.DOM (Element)
import Web.Event.Event (Event, EventType(..))
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent.EventTypes (dragover)
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

component :: forall m. MonadAff m => H.Component (HTML Action) Unit Void m
component = 
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = \_ -> pure Nothing
        , receive = const Nothing
        , initialize = Nothing
        , finalize = Nothing
        }
    }

initialState :: Input -> Store
initialState _ = initialStore

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
    , HE.onMouseDown $ \event -> StartDrag index (MouseEvent.clientX event) (MouseEvent.clientY event)
    , HE.onMouseMove $ \event -> MoveDrag (MouseEvent.clientX event) (MouseEvent.clientY event)
    , HE.onMouseUp $ const EndDrag
    , HE.onDragOver $ const $ DragOver index
    , HP.draggable true
    ]
    [ HH.td_ [ HH.text row ] ]

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
    DragOver index ->
      H.modify_ \s -> s { dragOverIndex = Just index }
    EndDrag ->
      H.modify_ \s -> s { dragState = s.dragState { index = Nothing, isDragging = false }, dragOverIndex = Nothing }
    NoOp ->
      pure unit

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  halogenIO <- HD.runUI component unit body
  -- `halogenIO` is a HalogenIO record. You can interact with the running component using this record.
  -- If you don't need to interact with the component, you can ignore `halogenIO`.
  pure unit

moveRow :: Int -> Int -> Array String -> Array String
moveRow from to rows =
  let
    draggedRow = fromMaybe "" $ rows !! from
    rowsWithoutDragged = fromMaybe rows $ deleteAt from rows
  in
    fromMaybe rowsWithoutDragged $ insertAt to draggedRow rowsWithoutDragged