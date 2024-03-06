module Types.Player
  ( ActivePlayers(..)
  , DisplayPlayer
  , DisplayPlayers
  , Player
  , PlayerData
  , PlayerEntry
  , Players
  , PlayersMap(..)
  , RankingCSV
  , RequestFunction
  , SortOption
  , SortValue(..)
  , compareMaybes
  , decodeJsonPlayer
  , decodeJsonPlayerData
  , decodeOptionalField
  , encodePlayerRanking
  , encodePlayersWithRanking
  , parseRankingCSV
  , sortOptions
  , transformAndEncodeDisplayPlayers
  , transformToDisplayPlayers
  )
  where

import Prelude

import Affjax (Error, Request, Response)
import Data.Argonaut (encodeJson)
import Data.Argonaut.Core (Json, fromNumber, toObject, fromObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode.Encoders (encodeArray)
import Data.Array (mapWithIndex, sort, nub)
import Data.Either (Either(..))
import Data.Foldable (all, foldl)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.Common (split, replace)
import Data.String.Pattern (Replacement(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Foreign.Object (Object, lookup)
import Foreign.Object as Object
import Util.DraftUtils (normalizeAndSplitLines)


type RequestFunction = forall a. Request a -> Aff (Either Error (Response a))

type SortOption = String

sortOptions :: Array SortOption
sortOptions = ["ID", "Surname", "'23 Rank", "'23 Points"]

data SortValue
  = SortString String
  | SortNumber (Maybe Number)
  | SortInt (Maybe Int)

compareMaybes :: forall a. Ord a => Maybe a -> Maybe a -> Ordering
compareMaybes Nothing Nothing = EQ
compareMaybes Nothing (Just _) = LT
compareMaybes (Just _) Nothing = GT
compareMaybes (Just a) (Just b) = compare a b

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

transformToDisplayPlayers :: PlayersMap -> DisplayPlayers
transformToDisplayPlayers (PlayersMap playersMap) =
  map toDisplayPlayer $ Map.toUnfoldable playersMap
  where
    toDisplayPlayer (Tuple key player) = {
        id: key
      , active: player.active
      , batSide: player.batSide
      , currentTeam: player.currentTeam
      , nameSlug: player.nameSlug
      , pitchHand: player.pitchHand
      , playerId: player.playerId
      , primaryPosition: player.primaryPosition
      , useLastName: player.useLastName
      , useName: player.useName
      , past_ranking: player.past_ranking
      , past_fpts: player.past_fpts
      , future_fpts: player.future_fpts 
      , future_ranking: player.future_ranking
      , displayOrder: 1800
    }

type PlayerData = 
  { checksum :: String
  , dataPulled :: String
  , officialPlayers :: PlayersMap
  }

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

-- new parts
type PlayerRanking = { playerId :: Int, ranking :: Int }

transformAndEncodeDisplayPlayers :: DisplayPlayers -> Json
transformAndEncodeDisplayPlayers players =
  let playerRankings = mapWithIndex (\index player -> encodePlayerRanking player (index + 1)) players
  in encodeJson playerRankings 

validatePlayerIds :: Array DisplayPlayer -> Boolean
validatePlayerIds players = all (\player -> player.playerId > 0) players

validateRankings :: Array Int -> Boolean
validateRankings rankings =
  let sortedRankings = sort rankings
      uniqueRankings = nub sortedRankings
  in sortedRankings == uniqueRankings && all (\i -> i > 0) sortedRankings

encodePlayersWithRanking :: Array (Tuple DisplayPlayer Int) -> Either String Json
encodePlayersWithRanking playerRankings =
  if validatePlayerIds (map Tuple.fst playerRankings) && validateRankings (map Tuple.snd playerRankings)
    then Right $ encodeArray (uncurry encodePlayerRanking) playerRankings
    else Left "Validation failed: Player IDs must be positive and rankings must be unique and sequential."

encodePlayerRanking :: DisplayPlayer -> Int -> Json
encodePlayerRanking player ranking = 
  let
    -- Create individual Json values for playerId and ranking
    playerIdJson = fromNumber $ toNumber player.playerId
    rankingJson = fromNumber $ toNumber ranking

    -- Construct the Object Json with key-value pairs
    playerObj = Object.empty
                 # Object.insert "playerId" playerIdJson
                 # Object.insert "ranking" rankingJson
  in
  fromObject playerObj