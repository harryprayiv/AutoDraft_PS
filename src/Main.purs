-- | module attempting to replicate functionality from https://codepen.io/chingy/pen/Exxvpjo using Halogen and Purescript
module Main where

import Prelude

import Affjax as H
import Data.Array (deleteAt, insertAt, length, mapWithIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, modify_) as H
import Halogen as HA
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
  , originalMouseX :: Int
  , originalMouseY :: Int
  , currentMouseX :: Int
  , currentMouseY :: Int
  , isDragging :: Boolean
  , placeholderIndex :: Maybe Int
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
  , dragState: { index: Nothing, originalMouseX: 0, originalMouseY: 0, currentMouseX: 0, currentMouseY: 0, isDragging: false, placeholderIndex: Nothing }
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
        , HH.tbody_ $ mapWithIndex (\index (Tuple name occupation) -> renderRow index name occupation state) state.rows
        ]
    ]

renderRow :: forall m. Int -> String -> String -> Store -> H.ComponentHTML Action () m
renderRow index name occupation state =
  let
    isDragging = Just index == state.dragState.index
    isPlaceholder = Just index == state.dragState.placeholderIndex && not isDragging
  in
    HH.tr
      [ HP.classes $ ClassName <$> ["draggable-table__row"] <>
          (if isDragging then ["is-dragging"] else []) <>
          (if isPlaceholder then ["placeholder"] else [])
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
handleAction action = case action of
  StartDrag index x y -> 
    H.modify_ \s -> s { dragState = { index: Just index, originalMouseX: x, originalMouseY: y, currentMouseX: x, currentMouseY: y, isDragging: true, placeholderIndex: Just index }}
  MoveDrag x y -> 
    H.modify_ \s -> 
      let placeholderIndex = calculateNewPlaceholderIndex x y s
      in s { dragState = s.dragState { currentMouseX = x, currentMouseY = y, placeholderIndex = placeholderIndex }}
  DragOver index ->
    H.modify_ \s -> s { dragState = s.dragState { placeholderIndex = Just index }}
  EndDrag -> do
    newState <- H.get
    let dragState = newState.dragState
    case dragState.index of
      Just fromIndex -> case dragState.placeholderIndex of
        Just toIndex -> H.modify_ \s -> s { rows = moveRow fromIndex toIndex s.rows, dragState = resetDragState }
        Nothing -> H.modify_ \s -> s { dragState = resetDragState }
      Nothing -> pure unit
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

-- Function to update the placeholder index based on the mouse's position
updatePlaceholderIndex :: Int -> Int -> Store -> Effect (Maybe Int)
updatePlaceholderIndex mouseX mouseY store = do
  let
    numRows = length store.rows
    draggedRowIndex = fromMaybe 0 store.dragState.index
    calculateNewIndex i = if i < numRows
      then do
        rowTop <- runEffectFn1 DomUtils.getRowTop i
        rowHeight <- runEffectFn1 DomUtils.getRowHeight i
        let rowMid = rowTop + (rowHeight / 2)
        if mouseY > rowMid
          then calculateNewIndex (i + 1)
          else pure (Just i)
      else pure (Just (numRows - 1))
  calculateNewIndex draggedRowIndex

findDropIndex :: Store -> Maybe Int
findDropIndex store = store.dragOverIndex

calculateNewPlaceholderIndex :: Int -> Int -> Store -> Maybe Int
calculateNewPlaceholderIndex mouseX mouseY store =
  let
    rowHeight = 30 -- Assume a fixed height for each row
    offsetY = mouseY - store.dragState.originalMouseY
    draggedRowIndex = fromMaybe 0 store.dragState.index
    numRows = length store.rows
    newIndex = draggedRowIndex + (offsetY `div` rowHeight)
  in
    if newIndex < 0 then Just 0
    else if newIndex >= numRows then Just (numRows - 1)
    else Just newIndex


findDropIndex :: Store -> Maybe Int
findDropIndex store =
  -- Implement logic to determine the new index for the dropped row
  -- This might be based on mouse position or the `dragOverIndex`
  store.dragOverIndex    

-- Function to reset the drag state
resetDragState :: DragState
resetDragState = 
  { index: Nothing, originalX: 0, originalY: 0, currentX: 0, currentY: 0, isDragging: false, placeholderIndex: Nothing }