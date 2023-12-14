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
  , primaryPosition :: String
  , useLastName :: String
  , useName :: String
  , past_ranking :: Maybe Number
  , past_fpts :: Maybe Number
  , future_fpts :: Maybe Number
  , future_ranking :: Maybe Number
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
  primaryPosition <- decodeField obj "primaryPosition"
  useLastName <- decodeField obj "useLastName"
  useName <- decodeField obj "useName"

  -- New fields with Maybe type
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