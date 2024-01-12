-- | module attempting to replicate functionality from https://codepen.io/chingy/pen/Exxvpjo using Halogen and Purescript
module Main where

import CSS.Property
import CSS.Selector
import CSS.Stylesheet
import CSS.Text.Transform
import Prelude
import Web.HTML.Common
-- import CSS (Rule(..), color, fromString, lighter, zIndex)
-- import CSS as CSS
-- import CSS.Background (backgroundColor, background)
-- import CSS.Border (border, borderTop, solid)
-- import CSS.Box (boxShadow)
-- import CSS.Color (rgba, white)
-- import CSS.Common (none)
-- import CSS.Cursor (cursor)
-- import CSS.Display (absolute, opacity, position)
-- import CSS.Font (GenericFontFamily(..), bold, fontFamily, fontSize, fontWeight, sansSerif, serif)
-- import CSS.Geometry (top, left, width, height, padding, margin)
-- import CSS.Geometry (top, left, width, height, padding, margin) as Geo
-- import CSS.Gradient (radialGradient, circle, closestSide)
-- import CSS.Property (Key(..), Literal(..), Prefixed(..), Value, value)
-- import CSS.Selector (element) as CSel
-- import CSS.Size (px)
-- import CSS.Size (px, pct, em)
-- import CSS.Stylesheet (key, rule, (?), CSS)
-- import CSS.Text (letterSpacing)
-- import CSS.Text.Transform (uppercase, capitalize)
-- import CSS.TextAlign (center, textAlign)
import Data.Array (deleteAt, fold, insertAt, mapWithIndex, splitAt, (!!))
import Data.Function (identity)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)

import Halogen (ClassName(..), ElemName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.Aff.Driver as H
import Halogen.HTML (HTML)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.CSS (stylesheet)
import Halogen.HTML.Elements (div, element)
import Halogen.HTML.Elements as HEL
import Halogen.HTML.Events as HEV
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT, updateStore)
import Halogen.Store.Select (selectEq, select, Selector) as Store
import Halogen.VDom.Driver (runUI)
import Halogen.VDom.Types (ElemName(..))
import Web.DOM (Element)
import Web.Event.Event (Event, EventType(..))
import Web.UIEvent.MouseEvent (MouseEvent(..), clientX, clientY, toEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as MouseEvent
import Styling (renderStylesheet)


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
  }

type Store = 
  { rows :: Array String
  , dragState :: DragState
  , dragOverIndex :: Maybe Int
  }

data Action
  = StartDrag Int Int Int  -- include the starting x and y positions
  | MoveDrag Int Int       -- include the new x and y positions
  | DragOver Int           -- include the index of the row being dragged over
  | EndDrag

initialStore :: Store
initialStore =  
  { rows: [ "April Douglas - Health Educator"
          , "Salma Mcbride - Mental Health Counselor"
          , "Kassandra Donovan - Makeup Artists"
          , "Yosef Hartman - Theatrical and Performance"
          , "Ronald Mayo - Plant Etiologist"
          , "Trey Woolley - Maxillofacial Surgeon"
          ]
  , dragState: { index: Nothing, originalX: 0, originalY: 0, currentX: 0, currentY: 0 }
  , dragOverIndex: Nothing
  }

component
  :: forall query output m
   . MonadStore Action Store m
  => H.Component query Input output m
component = connect selectStore $ H.mkComponent
  { initialState: deriveState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }

render :: forall m. MonadAff m => Store -> H.ComponentHTML Action () m
render state =
  HH.div []
    [ renderStylesheet
    , HH.table
        [ HP.class_ (HH.ClassName "draggable-table") ]
        [ HH.thead_
          [ HH.tr_
            [ HH.th_ [ HH.text "Name" ]
            , HH.th_ [ HH.text "Occupation" ]
            ]
          ]
        , HH.tbody_
            $ mapWithIndex (\index row -> renderRow index row state.dragging) state.rows
        ]
    ]

renderRow :: forall m. MonadAff m => Connected Store Input -> Int -> String -> Maybe Int -> H.ComponentHTML Action () m
renderRow connected index row dragging =
  HH.tr
    [ HP.classes $ ClassName <$> ["draggable-table__row"] <> 
        if Just index == connected.context.dragState.index then ["is-dragging"] 
        else if Just index == connected.context.dragOverIndex then ["drag-over"] 
        else []
    , HEV.onMouseDown $ \event -> handleMouseDownEvent (toEvent event) connected.dispatch
    , HEV.onMouseMove $ \event -> handleMouseMoveEvent event connected.dispatch
    , HEV.onMouseUp $ const $ connected.dispatch EndDrag
    , HEV.onDragOver $ const $ connected.dispatch $ DragOver index
    , HP.draggable true
    ]
    $ map renderCell (splitAt 15 row)
  where
    handleMouseDownEvent :: Event -> (Action -> Unit) -> Unit
    handleMouseDownEvent event dispatch = 
      case MouseEvent.fromEvent event of
        Just mouseEvent -> dispatch $ StartDrag index (MouseEvent.clientX mouseEvent) (MouseEvent.clientY mouseEvent)
        Nothing -> dispatch EndDrag -- Fallback in case of an event type mismatch

    handleMouseMoveEvent :: Event -> (Action -> Unit) -> Unit
    handleMouseMoveEvent event dispatch =
      case MouseEvent.fromEvent event of
        Just mouseEvent -> dispatch $ MoveDrag (MouseEvent.clientX mouseEvent) (MouseEvent.clientY mouseEvent)
        Nothing -> dispatch EndDrag -- Fallback in case of an event type mismatch


reduce :: Store -> Action -> Store
reduce store action = case action of
  StartDrag index x y -> 
    store { dragState = { index: Just index, originalX: x, originalY: y, currentX: x, currentY: y }}
  MoveDrag x y -> 
    store { dragState = store.dragState { currentX: x, currentY: y }}
  DragOver index -> 
    store { dragOverIndex = Just index }
  EndDrag -> 
    store { dragState = store.dragState { index: Nothing, currentX: 0, currentY: 0 }, dragOverIndex = Nothing }


renderCell :: forall m. MonadAff m => Int -> String -> H.ComponentHTML Action () m
renderCell index content = 
  HH.td 
    [ HP.class_ (HH.ClassName "table-cell") ]
    [ HH.text content ]  

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM (Connected Store Input) Action o m Unit
handleAction action = updateStore action
  case action of
    StartDrag index x y -> 
      updateStore $ StartDrag index x y
    MoveDrag x y -> 
      updateStore $ MoveDrag x y
    DragOver index -> 
      updateStore $ DragOver index
    EndDrag -> 
      updateStore EndDrag

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  root <- runStoreT initialStore reduce component
  void $ H.runUI root unit body

moveRow :: Int -> Int -> Array String -> Array String
moveRow from to rows =
  let
    draggedRow = fromMaybe "" $ rows !! from
    rowsWithoutDragged = fromMaybe rows $ deleteAt from rows
  in
    fromMaybe rowsWithoutDragged $ insertAt to draggedRow rowsWithoutDragged