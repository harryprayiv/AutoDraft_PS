module Styling
  ( 
  --   bodySelector
  -- , commonStyles
  -- , htmlSelector
  -- , myStylesheet
  renderStylesheet
  -- , textIndentStyle
  -- , userSelectStyle
  )
  where
import Prelude
import CSS.Property
import CSS.Selector
import CSS.Stylesheet
import CSS.Text.Transform
import Web.HTML.Common
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
import Data.Int (toNumber)
import Data.NonEmpty (NonEmpty, (:|))
import Halogen.HTML (HTML)
import Halogen.HTML.CSS (style)
import Halogen.HTML.CSS (stylesheet)
import Halogen.HTML.Elements (div, element)
import Halogen.HTML.Elements as HEL
import Halogen.HTML.Events as HEV
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT, updateStore)


-- commonStyles :: CSS
-- commonStyles = do
--   "html, body" ? do
--     padding (px 0)
--     margin (px 0)
--     width (pct 100)
--     height (pct 100)
--     backgroundColor (rgba 203 56 233 1)
--     background (radialGradient (circle closestSide) [ ((rgba 203 56 233 1) pct 0), ((rgba 132 47 168 1) pct 100) ])

userSelectStyle :: String -> CSS
userSelectStyle val = do
  star ? do
    key (Key (fromString "user-select")) val

textIndentStyle :: Int -> CSS
textIndentStyle val = do
  star ? do
    key (Key (fromString "text-indent")) (px (toNumber val))

-- -- Render the stylesheet in a Halogen component
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
    padding (px 0.0)
    margin (px 0.0)
    width (pct 100.0)
    height (pct 100.0)
    backgroundColor (rgba 203 56 233 1.0)
    -- background (radialGradient (circle closestSide) [ ((rgba 203 56 233 1.0) pct 0.0), ((rgba 132 47 168 1.0) pct 100) ])
    
  -- Apply styles to paragraphs
  element "p" ? do
    fontSize (em 0.75)
    fontWeight bold
    position absolute
    top (pct 15.0)
    width (pct 100.0)
    letterSpacing (px 5.0)
    textTransform uppercase
    textAlign center
    color white

  -- Apply styles to .drag-over class
  byClass "drag-over" ? do
    backgroundColor (rgba 220 220 220 1.0)

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