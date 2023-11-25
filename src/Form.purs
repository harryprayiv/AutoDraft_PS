module Form (WidgetNode, render, form) where

import Prelude
import Data.Array (foldl)
import Data.Maybe (Maybe(Just), maybe)

type InputType = String
type Label = String
type Method = String
type Action = String
type Name = String
type Value = String
type HTML = String

data Widget = Input InputType (Maybe Name) Label | Button Label | Form Method Action
data WidgetNode = WidgetNode Widget (Array WidgetNode)

submit :: Widget
submit = Button "Submit"

positionInput :: Widget
positionInput = Input "number" (Just "position") "Position"

formWidget :: Widget
formWidget = Form "post" "#"

data HTMLAttr = Attr Name Value

-- This function takes an HTMLAttr and returns its string representation
renderAttr :: HTMLAttr -> HTML
renderAttr (Attr name value) = " " <> name <> "=\"" <> value <> "\""

-- The foldAcc function is not needed anymore

tag :: HTML -> Name -> Array HTMLAttr -> HTML
tag content name attrs =
  let
    attrs' = foldl (\s attr -> s <> renderAttr attr) "" attrs
  in
    "<" <> name <> attrs' <> ">" <> content <> "</" <> name <> ">"

renderElement :: Widget -> String -> String
renderElement el ct =
  let
    tag' = tag ct
  in
    case el of
      Form mt act ->
        tag' "form" [Attr "method" mt, Attr "action" act]
      Input t n "" ->
        tag' "input" ([Attr "type" t] <> maybe [] (\n' -> [Attr "name" n']) n)
      Input t n lb ->
        tag (lb <> renderElement (Input t n "") "") "label" []
      Button lb ->
        tag' "button" [Attr "type" "submit", Attr "value" lb]

render :: WidgetNode -> String
render (WidgetNode el children) =
  let
    ct = foldl (\s child -> s <> render child) "" children
  in
    renderElement el ct

form :: WidgetNode
form = WidgetNode formWidget [WidgetNode positionInput [], WidgetNode submit []]
