-- | module attempting to replicate functionality from https://codepen.io/chingy/pen/Exxvpjo using Halogen and Purescript
module Main where

import Effect.Aff.Class (class MonadAff)
import Prelude
import Web.HTML.Common (ClassName(..))
import Data.Array (deleteAt, insertAt, mapWithIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Halogen.Store.Connect (Connected)
import Halogen.Store.Select as Store
import Halogen.VDom.Driver (runUI)
import Web.UIEvent.MouseEvent as MouseEvent
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML as HH
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, modify_) as H
import Halogen.Aff as HA

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

component :: forall q i m. MonadAff m => H.Component q Unit Void m
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

render :: forall m. MonadAff m => Store -> H.ComponentHTML Action () m
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