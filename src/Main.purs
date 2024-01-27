-- | module attempting to replicate functionality from https://codepen.io/chingy/pen/Exxvpjo using Halogen and Purescript
module Main where

import Prelude

import Data.Array (deleteAt, insertAt, mapWithIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, modify_) as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected)
import Halogen.Store.Select as Store
import Halogen.VDom.Driver (runUI)
import Web.HTML.Common (ClassName(..))
import Web.UIEvent.MouseEvent as MouseEvent

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
  = StartDrag Int Int Int
  | MoveDrag Int Int
  | DragOver Int
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
    , HE.onMouseDown $ \event -> StartDrag index (MouseEvent.clientX event) (MouseEvent.clientY event)
    , HE.onMouseMove $ \event -> MoveDrag (MouseEvent.clientX event) (MouseEvent.clientY event)
    , HE.onMouseUp $ const EndDrag
    , HE.onDragOver $ const $ DragOver index
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
    DragOver index ->
      H.modify_ \s -> s { dragOverIndex = Just index }
    EndDrag ->
      H.modify_ \s -> case s.dragState.index of
        Just fromIndex -> case findDropIndex s of
          Just toIndex -> s { rows = moveRow fromIndex toIndex s.rows
                            , dragState = resetDragState
                            , dragOverIndex = Nothing }
          Nothing -> s { dragState = resetDragState, dragOverIndex = Nothing }
        Nothing -> s
    NoOp ->
      pure unit

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

findDropIndex :: Store -> Maybe Int
findDropIndex store =
  -- Implement logic to determine the new index for the dropped row
  -- This might be based on mouse position or the `dragOverIndex`
  store.dragOverIndex    

-- Function to reset the drag state
resetDragState :: DragState
resetDragState = 
  { index: Nothing, originalX: 0, originalY: 0, currentX: 0, currentY: 0, isDragging: false }