module Main where

import Prelude

import Data.Either (Either(..), hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Yoga.JSON as JSON

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
  }

type Players = Map String Player

type ActivePlayers =
  { checksum :: String
  , dataPulled :: String
  , officialPlayers :: Players
  }

main :: Effect Unit
main = launchAff_ $ do
  fileContent <- readTextFile UTF8 "./appData/rosters/activePlayers.json"
  let parsedData = fromMaybe { checksum: "", dataPulled: "", officialPlayers: Map.empty } $ hush $ JSON.readJSON fileContent
  logShow (parsedData :: ActivePlayers)
