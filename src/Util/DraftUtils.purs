module Util.DraftUtils
  ( PosLookup
  , TeamLookup
  , getPositionDisplayValue
  , getTeamDisplayValue
  , normalizeAndSplitLines
  , position
  , showAsInt
  , spyShow
  , teams
  )
  where

import Prelude

import Data.Array (find)
import Data.Int (trunc)
import Data.Maybe (maybe)
import Data.String (Pattern(..))
import Data.String.Common (split, replace)
import Data.String.Pattern (Replacement(..))
import Data.Tuple (Tuple(..), fst)
import Debug (class DebugWarning, spyWith)

type PosLookup = Tuple String String 

position :: Array PosLookup
position = 
  [ Tuple "P" "1"
  , Tuple "C" "2"
  , Tuple "1B" "3"
  , Tuple "2B" "4"
  , Tuple "3B" "5"
  , Tuple "SS" "6"
  , Tuple "LF" "7"
  , Tuple "CF" "8"
  , Tuple "RF" "9"
  , Tuple "DH" "10"
  , Tuple "PH" "11"
  , Tuple "PR" "12"
  , Tuple "UN" "13"
  , Tuple "O" "O"
  , Tuple "Switch" "Y"
  ]

type TeamLookup = Tuple String Int 

teams :: Array TeamLookup
teams = 
  [ Tuple "LAA" 108
  , Tuple "ARI" 109
  , Tuple "BAL" 110
  , Tuple "BOS" 111
  , Tuple "CHC" 112
  , Tuple "CIN" 113
  , Tuple "CLE" 114
  , Tuple "COL" 115
  , Tuple "DET" 116
  , Tuple "HOU" 117
  , Tuple "KC" 118
  , Tuple "LAD" 119
  , Tuple "WSH" 120
  , Tuple "NYM" 121
  , Tuple "OAK" 133
  , Tuple "PIT" 134
  , Tuple "SD" 135
  , Tuple "SEA" 136
  , Tuple "SF" 137
  , Tuple "STL" 138
  , Tuple "TB" 139
  , Tuple "TEX" 140
  , Tuple "TOR" 141
  , Tuple "MIN" 142
  , Tuple "PHI" 143
  , Tuple "ATL" 144
  , Tuple "CWS" 145
  , Tuple "MIA" 146
  , Tuple "NYY" 147
  , Tuple "MIL" 158
  ]

spyShow :: DebugWarning => forall a. Show a => String -> a -> a
spyShow msg = spyWith msg show

showAsInt :: Number -> String
showAsInt num = show $ trunc num

getPositionDisplayValue :: String -> String
getPositionDisplayValue value =
  maybe value fst (find (\(Tuple _ code) -> code == value) position)

getTeamDisplayValue :: Int -> String
getTeamDisplayValue value =
  maybe (show value) fst (find (\(Tuple _ code) -> code == value) teams)


normalizeAndSplitLines :: String -> Array String
normalizeAndSplitLines content =
  let
    normalizedContent = replace (Pattern "\r\n") (Replacement "\n") $
                        replace (Pattern "\r") (Replacement "\n") content
  in
    split (Pattern "\n") normalizedContent
