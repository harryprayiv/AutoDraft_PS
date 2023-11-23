module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (launchAff_)
import Node.FS.Aff (readTextFile)
import Yoga.JSON as JSON
import Node.Encoding (Encoding(..))
import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Effect.Class.Console (logShow)

type Player = { first_name :: String, last_name :: String }

main :: Effect Unit
main = launchAff_ $ do
  fileContent <- readTextFile UTF8 "/home/bismuth/plutus/workspace/devWs/simpleDraft/src/activePlayers.json"
  let players = fromMaybe [] $ hush $ JSON.readJSON fileContent
  logShow (players :: Array Player)
