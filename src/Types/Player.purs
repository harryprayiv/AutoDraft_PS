module Types.Player
  ( ActivePlayers(..)
  , DisplayPlayer
  , DisplayPlayers
  , Player
  , PlayerData
  , PlayerEntry
  , Players
  , PlayersMap(..)
  , Query(..)
  , RankingCSV
  , RequestFunction
  , SortOption
  , SortValue(..)
  , State
  , compareMaybes
  , decodeField
  , decodeJsonPlayer
  , decodeJsonPlayerData
  , parseRankingCSV
  , unwrapPlayersMap
  )
  where

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Affjax (Error, Request, Response)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.Common (split, replace)
import Data.String.Pattern (Replacement(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, lookup)
import Foreign.Object as Object
import Util.DraftUtils (normalizeAndSplitLines)
import Data.Int (toNumber)
import Effect.Aff (Aff)

type RequestFunction = forall a. Request a -> Aff (Either Error (Response a))

data Query a = GetState (State -> a)

type SortOption = String

data SortValue
  = SortString String
  | SortNumber (Maybe Number)
  | SortInt (Maybe Int)

instance eqSortValue :: Eq SortValue where
  eq x y = case (Tuple x y) of
    (Tuple (SortString a) (SortString b)) -> a == b
    (Tuple (SortNumber ma) (SortNumber mb)) -> ma == mb
    (Tuple (SortInt ma) (SortInt mb)) -> ma == mb
    _ -> false

instance ordSortValue :: Ord SortValue where
  compare x y = case (Tuple x y) of
    (Tuple (SortString a) (SortString b)) -> compare a b
    (Tuple (SortNumber ma) (SortNumber mb)) -> compareMaybes ma mb
    (Tuple (SortInt ma) (SortInt mb)) -> compareMaybes (map toNumber ma) (map toNumber mb)
    _ -> EQ


compareMaybes :: forall a. Ord a => Maybe a -> Maybe a -> Ordering
compareMaybes Nothing Nothing = EQ
compareMaybes Nothing (Just _) = GT
compareMaybes (Just _) Nothing = LT
compareMaybes (Just a) (Just b) = compare a b


type State = {
    allPlayers :: PlayersMap
  , displayPlayers :: DisplayPlayers
  , filterInputs :: Array String
  , currentSort :: SortOption
  , loading :: Boolean
  , error :: Maybe String
  , sortChangeFlag :: Boolean
  , sortOrder :: Boolean
  , dragIndex :: Maybe Int
  , dropIndex :: Maybe Int
  , manualOrdering :: Boolean 
}


type DisplayPlayer = {
    id :: String
  , active :: Boolean
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
  , displayOrder :: Int
}

type DisplayPlayers = Array DisplayPlayer

type PlayerData = 
  { checksum :: String
  , dataPulled :: String
  , officialPlayers :: PlayersMap
  }

newtype PlayersMap = PlayersMap (Map String Player)

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

type RankingCSV = Array (Array String)

parseRankingCSV :: String -> RankingCSV
parseRankingCSV content =
  let
    rows = normalizeAndSplitLines content
    cleanField = replace (Pattern "\r") (Replacement "")
    fields = map (\row -> map cleanField (split (Pattern ",") row)) rows
  in
    fields
