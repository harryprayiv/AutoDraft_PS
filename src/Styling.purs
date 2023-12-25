module Styling where

import Prelude

import CSS (backgroundColor, color) as Color
import CSS.Border as Border
import CSS.Color (black, white) as Color
import CSS.Geometry (paddingBottom, paddingLeft, paddingRight, paddingTop) as CSS
import CSS.Size as Size
import CSS.Stylesheet (CSS)

cellStyle :: CSS
cellStyle = do
  Border.border Border.solid (Size.px 1.0) Color.black
  CSS.paddingTop (Size.px 0.4)
  CSS.paddingBottom (Size.px 0.4)
  CSS.paddingLeft (Size.px 5.0)
  CSS.paddingRight (Size.px 5.0)

buttonStyle :: CSS
buttonStyle = do
  Border.border Border.solid (Size.px 1.0) Color.white
  CSS.paddingTop (Size.px 10.0)
  CSS.paddingBottom (Size.px 10.0)
  CSS.paddingLeft (Size.px 10.0)
  CSS.paddingRight (Size.px 10.0)

activeButtonStyle :: CSS
activeButtonStyle = do
  Border.borderColor Color.black
  Color.backgroundColor Color.white
  Color.color Color.white  


resetButtonStyle :: CSS
resetButtonStyle = do
  -- Define reset button styles
  Border.border Border.solid (Size.px 1.0) Color.black
  CSS.paddingTop (Size.px 10.0)
  CSS.paddingBottom (Size.px 10.0)
  CSS.paddingLeft (Size.px 10.0)
  CSS.paddingRight (Size.px 10.0)
  -- Cursor.pointer