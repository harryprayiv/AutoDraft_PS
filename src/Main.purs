-- | module attempting to replicate functionality from https://codepen.io/chingy/pen/Exxvpjo using Halogen and Purescript
module Main where

import Prelude
import Web.HTML.Common
import CSS.Property
import CSS.Selector
import CSS.Stylesheet
import CSS.Text.Transform
import CSS (Rule(..), color, fromString, lighter, zIndex)
import CSS as CSS
import CSS.Background (backgroundColor, background)
import CSS.Border (border, borderTop, solid)
import CSS.Box (boxShadow)
import CSS.Color (rgba, white)
import CSS.Common (none)
import CSS.Cursor (cursor)
import CSS.Display (absolute, opacity, position)
import CSS.Font (GenericFontFamily(..), bold, fontFamily, fontSize, fontWeight, sansSerif, serif)
import CSS.Geometry (top, left, width, height, padding, margin)
import CSS.Gradient (radialGradient, circle, closestSide)
import CSS.Property (Key(..), Literal(..), Prefixed(..), Value, value)
import CSS.Selector (element) as CSel
import CSS.Size (px)
import CSS.Size (px, pct, em)
import CSS.Stylesheet (key, rule, (?), CSS)
import CSS.Text (letterSpacing)
import CSS.Text.Transform (uppercase, capitalize)
import CSS.TextAlign (center, textAlign)
import Data.Array (deleteAt, fold, insertAt, mapWithIndex, splitAt, (!!))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (Pattern(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Halogen (ClassName(..), ElemName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.CSS (stylesheet)
import Halogen.HTML.Elements (div, element)
import Halogen.HTML.Elements as HEL
import Halogen.HTML.Events as HEV
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Halogen.VDom.Types (ElemName(..))
import Web.DOM (Element)
import Web.Event.Event (Event, EventType(..))
import Web.UIEvent.MouseEvent (MouseEvent(..), clientX, clientY, toEvent)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)
import Web.UIEvent.MouseEvent as DOM
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as MouseEvent

type DragState = 
  { index :: Maybe Int
  , originalX :: Int
  , originalY :: Int
  , currentX :: Int
  , currentY :: Int
  }

type State = 
  { rows :: Array String
  , dragState :: DragState
  , dragOverIndex :: Maybe Int
  }

data Action
  = StartDrag Int Int Int  -- include the starting x and y positions
  | MoveDrag Int Int       -- include the new x and y positions
  | DragOver Int           -- include the index of the row being dragged over
  | EndDrag

initialState :: State
initialState =
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

component :: forall q i o m. H.Component HH.HTML q i o m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
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

-- renderRow :: forall m. MonadAff m => State -> Int -> String -> Maybe Int -> H.ComponentHTML Action () m
-- renderRow state index row dragging = 
--   HH.tr
--     [ HP.classes $ ClassName <$> ["draggable-table__row"] <> 
--         if Just index == state.dragState.index then ["is-dragging"] 
--         else if Just index == state.dragOverIndex then ["drag-over"] 
--         else []
--     , HEV.onMouseDown $ \event -> handleMouseDown index event
--     , HEV.onMouseMove $ \event -> case DOM.fromEvent event of --Could not match type MouseEvent with type Event
--         Just mouseEvent -> handleMouseMove mouseEvent
--         Nothing -> EndDrag -- Fallback in case of an event type mismatch
--     , HEV.onMouseUp $ const EndDrag
--     , HEV.onDragOver $ const $ DragOver index
--     , HP.draggable true
--     ]
--     $ map renderCell (splitAt 15 row)

renderRow :: forall m. MonadAff m => State -> Int -> String -> Maybe Int -> H.ComponentHTML Action () m
renderRow state index row dragging = 
  HH.tr
    [ HP.classes $ ClassName <$> ["draggable-table__row"] <> 
        if Just index == state.dragState.index then ["is-dragging"] 
        else if Just index == state.dragOverIndex then ["drag-over"] 
        else []
    , HEV.onMouseDown $ \event -> case DOM.fromEvent event of
        Just mouseEvent -> handleMouseDown index mouseEvent
        Nothing -> EndDrag  -- Provide a default action as a fallback
    , HEV.onMouseMove $ \event -> case DOM.fromEvent event of
        Just mouseEvent -> handleMouseMove mouseEvent
        Nothing -> EndDrag -- Fallback in case of an event type mismatch
    , HEV.onMouseUp $ const EndDrag
    , HEV.onDragOver $ const $ DragOver index
    , HP.draggable true
    ]
    $ map renderCell (splitAt 15 row)


handleMouseDown :: Int -> MouseEvent -> Action
handleMouseDown index mouseEvent =
  StartDrag index (DOM.clientX mouseEvent) (DOM.clientY mouseEvent)

handleMouseMove :: MouseEvent -> Action
handleMouseMove mouseEvent =
  MoveDrag (DOM.clientX mouseEvent) (DOM.clientY mouseEvent)

renderCell :: forall m. MonadAff m => Int -> String -> H.ComponentHTML Action () m
renderCell index content = 
  HH.td 
    [ HP.class_ (HH.ClassName "table-cell") ]
    [ HH.text content ]  

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action o m Unit
handleAction action = 
  case action of
    StartDrag index x y -> 
      H.modify_ (\s -> s { dragState = { index: Just index, originalX: x, originalY: y, currentX: x, currentY: y }})
    MoveDrag x y -> 
      H.modify_ (\s -> s { dragState = s.dragState { currentX: x, currentY: y }})
    DragOver index -> 
      H.modify_ (\s -> s { dragOverIndex = Just index })
    EndDrag -> 
      H.modify_ (\s -> s { dragState = initialState.dragState, dragOverIndex = Nothing })

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

moveRow :: Int -> Int -> Array String -> Array String
moveRow from to rows =
  let
    draggedRow = fromMaybe "" $ rows !! from
    rowsWithoutDragged = fromMaybe rows $ deleteAt from rows
  in
    fromMaybe rowsWithoutDragged $ insertAt to draggedRow rowsWithoutDragged

-- Common CSS rules
commonStyles :: CSS
commonStyles = do
  "html, body" ? do
    padding (px 0)
    margin (px 0)
    width (pct 100)
    height (pct 100)
    backgroundColor (rgba 203 56 233 1)
    background (radialGradient (circle closestSide) [ ((rgba 203 56 233 1) pct 0), ((rgba 132 47 168 1) pct 100) ])

userSelectStyle :: String -> CSS
userSelectStyle val = do
  star ? do
    key (Key (fromString "user-select")) val

textIndentStyle :: Int -> CSS
textIndentStyle val = do
  star ? do
    key (Key (fromString "text-indent")) (px (toNumber val))

-- Render the stylesheet in a Halogen component
renderStylesheet :: forall p i. HTML p i
renderStylesheet = div [] [ myStylesheet ]

htmlSelector :: Selector
htmlSelector = CSel.element "html"

bodySelector :: Selector
bodySelector = CSel.element "body"

myStylesheet :: forall p i. HTML p i
myStylesheet = stylesheet $ do
  -- Apply styles to all elements
  star ? do
    fontFamily [] (serif :| [sansSerif])

  -- Apply styles to html and body using deep selector composition
  deep htmlSelector bodySelector ? do
    padding (px 0)
    margin (px 0)
    width (pct 100)
    height (pct 100)
    backgroundColor (rgba 203 56 233 1)
    background (radialGradient (circle closestSide) [ ((rgba 203 56 233 1) pct 0), ((rgba 132 47 168 1) pct 100) ])
    
  -- Apply styles to paragraphs
  element "p" ? do
    fontSize (em 0.75)
    fontWeight bold
    position absolute
    top (pct 15)
    width (pct 100)
    letterSpacing (px 5)
    textTransform uppercase
    textAlign center
    color white

  -- Apply styles to .drag-over class
  byClass "drag-over" ? do
    backgroundColor (rgba 220 220 220 1)

  -- Apply styles to .draggable-table class
  byClass "draggable-table" ? do
    position absolute
    top (pct 25)
    left (pct 20)
    width (pct 60)
    height (pct 50)
    boxShadow "0px 0px 10px 8px rgba(0, 0, 0, 0.1)"
    backgroundColor white

    -- Nested styles for .draggable-table__drag class
    byClass "draggable-table__drag" ? do
      fontSize (em 0.95)
      fontWeight lighter
      textTransform capitalize
      position absolute
      width (pct 100)
      border (px 1) solid (rgba 241 241 241 1)
      zIndex 10
      cursor "grabbing"
      boxShadow "2px 2px 3px 0px rgba(0, 0, 0, 0.05)"
      opacity 1

  -- Apply styles to table heads
  deep (byClass "draggable-table") (element "thead") ? do
    element "th" ? do
      height (px 25)
      fontWeight bold
      textTransform capitalize
      padding (px 10)

  -- Apply styles to table body
  deep (byClass "draggable-table") (element "tbody") ? do
    element "tr" ? do
      cursor "grabbing"

      element "td" ? do
        fontSize (em 0.95)
        fontWeight lighter
        textTransform capitalize
        padding (px 10)
        borderTop (px 1) solid (rgba 245 245 245 1)

    element "tr:nth-child(even)" ? do
      backgroundColor (rgba 247 247 247 1)

    element "tr:nth-child(odd)" ? do
      backgroundColor white

    element "tr.is-dragging" ? do
      backgroundColor (rgba 241 196 15 1)
      element "td" ? do
        color (rgba 255 230 131 1)