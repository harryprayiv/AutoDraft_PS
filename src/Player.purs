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
  , filterPlayers
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
  , playerId :: Int
  , primaryPosition :: String
  , useLastName :: String
  , useName :: String
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
  -- Decode each field using decodeField
  active <- decodeField obj "active"
  batSide <- decodeField obj "batSide"
  currentTeam <- decodeField obj "currentTeam"
  playerId <- decodeField obj "playerId"
  primaryPosition <- decodeField obj "primaryPosition"
  useLastName <- decodeField obj "useLastName"
  useName <- decodeField obj "useName"

  pure
    { active: active
    , batSide: batSide
    , currentTeam: currentTeam
    , playerId: playerId
    , primaryPosition: primaryPosition
    , useLastName: useLastName
    , useName: useName
    }

decodeField :: forall a. DecodeJson a => Object Json -> String -> Either JsonDecodeError a
decodeField obj fieldName = case lookup fieldName obj of
  Just value -> decodeJson value
  Nothing -> Left MissingValue

filterPlayers :: String -> Map String Player -> Map String Player
filterPlayers positionInput players = 
  Map.filter (\player -> player.primaryPosition == positionInput) players

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