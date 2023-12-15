module CSVParser
  ( RankingCSV
  , parseRankingCSV
  )
  where

import Prelude
import Data.String (Pattern(..))
import Data.String.Common (split)

type RankingCSV = Array (Array String)

parseRankingCSV :: String -> RankingCSV
parseRankingCSV content =
  let
    rows = split (Pattern "\n") content
    fields = map (split (Pattern ",")) rows
  in
    fields
