module CSVParser
  ( CSV
  , parseCSV
  )
  where

import Prelude
import Data.String (Pattern(..))
import Data.String.Common (split)

type CSV = Array (Array String)

parseCSV :: String -> CSV
parseCSV content =
  let
    rows = split (Pattern "\n") content
    fields = map (split (Pattern ",")) rows
  in
    fields
