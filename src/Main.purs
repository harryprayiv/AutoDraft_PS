-- | module attempting to replicate functionality from https://codepen.io/chingy/pen/Exxvpjo using Halogen and Purescript

module Main where

import CSS.Property
import CSS.Selector
import CSS.Stylesheet
import CSS.Text.Transform
import Prelude
import Web.HTML.Common
import CSS (Rule(..), color, lighter, zIndex)
import CSS as CSS
import CSS.Background (backgroundColor, background)
import CSS.Border (border, borderTop, solid)
import CSS.Box (boxShadow)
import CSS.Color (rgba, white)
import CSS.Common (none)
import CSS.Cursor (cursor)
import CSS.Display (absolute, opacity, position)
import CSS.Font (bold, fontFamily, fontSize, fontWeight)
import CSS.Geometry (top, left, width, height, padding, margin)
import CSS.Gradient (radialGradient, circle, closestSide)
import CSS.Property (Key(..), Literal(..), Prefixed(..), Value, value)
import CSS.Size (px, pct, em)
import CSS.Stylesheet (key, rule, (?), CSS)
import CSS.Text (letterSpacing)
import CSS.Text.Transform (uppercase, capitalize)
import CSS.TextAlign (center, textAlign)
import Data.Array (deleteAt, fold, insertAt, mapWithIndex, splitAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.CSS (stylesheet)
import Halogen.HTML.CSS (style)
import Halogen.HTML.Elements (div, element)
import Halogen.HTML.Elements as HE
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.DOM (Element)
import CSS.Property (value, Value, Literal(..))
import CSS.Size (px)
import Data.String (Pattern(..))

type State = 
  { rows :: Array String
  , dragging :: Maybe Int
  }

data Action
  = StartDrag Int
  | DragOver Int 
  | EndDrag
  | Drop Int

initialState :: State
initialState =
  { rows: [ "April Douglas - Health Educator"
          , "Salma Mcbride - Mental Health Counselor"
          , "Kassandra Donovan - Makeup Artists"
          , "Yosef Hartman - Theatrical and Performance"
          , "Ronald Mayo - Plant Etiologist"
          , "Trey Woolley - Maxillofacial Surgeon"
          ]
  , dragging: Nothing
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

renderRow :: forall m. MonadAff m => Int -> String -> Maybe Int -> H.ComponentHTML Action () m
renderRow index row dragging =
  HH.tr
    [ HP.classes $ ClassName <$> ["draggable-table__row"] <> if dragging == Just index then ["is-dragging"] else []
    , fold $ userSelectStyle "none" <> textIndentStyle "50px"
    , HE.onDragStart $ HE.input (StartDrag index)
    , HE.onDragEnd $ HE.input EndDrag
    , HP.draggable true
    ]
    $ map renderCell (splitAt 15 row)
  where
    renderCell :: String -> H.ComponentHTML Action () m
    renderCell content = HH.td_ [ HH.text content ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action o m Unit
handleAction action = case action of
  StartDrag index ->
    H.modify_ (\state -> state { dragging = Just index })
  DragOver index ->
    pure unit
  EndDrag ->
    H.modify_ (\state -> state { dragging = Nothing })
  Drop index -> do
    oldState <- H.get
    case oldState.dragging of
      Just draggingIndex -> do
        let updatedRows = moveRow draggingIndex index oldState.rows
        H.modify_ (\state -> state { rows = updatedRows, dragging = Nothing })
      Nothing ->
        pure unit

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


moveRow :: Int -> Int -> Array String -> Array String
moveRow from to rows =
  let
    draggedRow = fromMaybe "" $ rows !! from
    rowsWithoutDragged = deleteAt from rows
  in
    insertAt to draggedRow rowsWithoutDragged

-- CSS rules
myStylesheet :: forall p i. HTML p i
myStylesheet = stylesheet $ do
  "*" ? do
    fontFamily ["'Source Sans Pro'", "sans-serif"]
  "html, body" ? do
    padding (px 0)
    margin (px 0)
    width (pct 100)
    height (pct 100)
    backgroundColor (rgba 203 56 233 1)
    background (radialGradient (circle closestSide) [ ((rgba 203 56 233 1) pct 0), ((rgba 132 47 168 1) pct 100) ])
  -- Paragraphs
  "p" ? do
    fontSize (em 0.75)
    fontWeight bold
    position absolute
    top (pct 15)
    width (pct 100)
    letterSpacing (px 5)
    textTransform uppercase
    textAlign center
    color white


  -- Draggable Table
  ".draggable-table" ? do
    position absolute
    top (pct 25)
    left (pct 20)
    width (pct 60)
    height (pct 50)
    boxShadow "0px 0px 10px 8px rgba(0, 0, 0, 0.1)"
    backgroundColor white

    -- Draggable Table Drag
    ".draggable-table__drag" ? do
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

    -- Table Head
    "thead" ? do
      "th" ? do
        height (px 25)
        fontWeight bold
        textTransform capitalize
        padding (px 10)

    -- Table Body
    "tbody" ? do
      "tr" ? do
        cursor "grabbing"

        "td" ? do
          fontSize (em 0.95)
          fontWeight lighter
          textTransform capitalize
          padding (px 10)
          borderTop (px 1) solid (rgba 245 245 245 1)

      "tr:nth-child(even)" ? do
        backgroundColor (rgba 247 247 247 1)

      "tr:nth-child(odd)" ? do
        backgroundColor white

      "tr.is-dragging" ? do
        backgroundColor (rgba 241 196 15 1)
        "td" ? do
          color (rgba 255 230 131 1)

        "td" ? do
          color (rgba 255 230 131 1)

-- Define a function to create a style rule for user-select
userSelectStyle :: String -> CSS
userSelectStyle val = do
  -- Use the property directly without Selector
  "user-select" ? value val

-- Define a function to create a style property for text-indent
textIndentStyle :: forall r i. Int -> HP.IProp (style :: String | r) i
textIndentStyle val = style $ do
  -- Use the property directly without Literal
  "text-indent" ? px val

-- Render the stylesheet in a Halogen component
renderStylesheet :: forall p i. HTML p i
renderStylesheet = div [] [ myStylesheet ]