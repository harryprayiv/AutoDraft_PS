module Player
  ( ActivePlayers(..)
  , Player
  , PlayerData
  , PlayerEntry
  , Players
  , PlayersMap(..)
  , decodeField
  , decodeJsonPlayer
  , decodeJsonPlayerData
  , position
  , teams
  , unwrapPlayersMap
  )
  where

import Prelude
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Core (Json, toObject)
import Data.Either (Either(..))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, lookup)
import Foreign.Object as Object


type PosLookup = Tuple String String 

position :: Array PosLookup
position = 
  [ Tuple "All" ""
  , Tuple "P" "1"
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

type PlayerData = 
  { checksum :: String
  , dataPulled :: String
  , officialPlayers :: PlayersMap
  }

type Player = 
  { active :: Boolean
  , batSide :: String
  , currentTeam :: Int
  , nameSlug :: String
  , pitchHand :: String
  , playerId :: Int
  , primaryPosition :: String
  , useLastName :: String
  , useName :: String
  , past_ranking :: Maybe Int
  , past_fpts :: Maybe Number
  , future_fpts :: Maybe Number
  , future_ranking :: Maybe Int
  }

newtype PlayersMap = PlayersMap (Map String Player)

type PlayerEntry = { key :: String, playerJson :: Json }

type Players = Map.Map String Player

newtype ActivePlayers = ActivePlayers PlayersMap

instance decodeJsonActivePlayers :: DecodeJson ActivePlayers where
  decodeJson json = do
    obj <- case toObject json of
      Just o -> pure o
      Nothing -> Left $ TypeMismatch "Expected an object"
    playersMap <- obj .: "officialPlayers"
    pure $ ActivePlayers playersMap

decodeJsonPlayerData :: Json -> Either JsonDecodeError PlayerData
decodeJsonPlayerData json = do
  obj <- case toObject json of
    Just o -> Right o
    Nothing -> Left $ TypeMismatch "Expected an object"

  checksum <- decodeField obj "checksum"
  dataPulled <- decodeField obj "dataPulled"
  playersMapWrapped <- obj .: "officialPlayers" >>= decodeJson

  pure
    { checksum: checksum
    , dataPulled: dataPulled
    , officialPlayers: playersMapWrapped
    }

decodeJsonPlayer :: Json -> Either JsonDecodeError Player
decodeJsonPlayer json = do
  obj <- case toObject json of
    Just o -> Right o
    Nothing -> Left $ TypeMismatch "Invalid JSON structure for Player"

  active <- decodeField obj "active"
  batSide <- decodeField obj "batSide"
  currentTeam <- decodeField obj "currentTeam"
  nameSlug <- decodeField obj "nameSlug"
  pitchHand <- decodeField obj "pitchHand"
  playerId <- decodeField obj "playerId"
  primaryPosition <- decodeField obj "primaryPosition"
  useLastName <- decodeField obj "useLastName"
  useName <- decodeField obj "useName"

  future_fpts <- decodeOptionalField obj "future_fpts"
  future_ranking <- decodeOptionalField obj "future_ranking"
  past_ranking <- decodeOptionalField obj "past_ranking"
  past_fpts <- decodeOptionalField obj "past_fpts"

  pure
    { active: active
    , batSide: batSide
    , currentTeam: currentTeam
    , nameSlug: nameSlug
    , pitchHand: pitchHand
    , playerId: playerId
    , primaryPosition: primaryPosition
    , useLastName: useLastName
    , useName: useName
    , future_fpts: future_fpts
    , future_ranking: future_ranking
    , past_ranking: past_ranking
    , past_fpts: past_fpts
    }

decodeOptionalField :: forall a. DecodeJson a => Object Json -> String -> Either JsonDecodeError (Maybe a)
decodeOptionalField obj fieldName = case lookup fieldName obj of
  Just value -> map Just (decodeJson value)
  Nothing -> Right Nothing

decodeField :: forall a. DecodeJson a => Object Json -> String -> Either JsonDecodeError a
decodeField obj fieldName = case lookup fieldName obj of
  Just value -> decodeJson value
  Nothing -> Left MissingValue

unwrapPlayersMap :: PlayersMap -> Map String Player
unwrapPlayersMap (PlayersMap map) = map

instance decodeJsonPlayersMap :: DecodeJson PlayersMap where
  decodeJson json = do
    obj <- case toObject json of
      Just o -> pure o
      Nothing -> Left $ TypeMismatch "Expected an object"
    let entries = Object.toUnfoldable obj :: Array (Tuple String Json)
    let buildMap acc (Tuple key playerJson) = 
          case decodeJsonPlayer playerJson of
            Right player -> Map.insert key player acc
            Left _ -> acc
    pure $ PlayersMap $ foldl buildMap Map.empty entries