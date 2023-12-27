module CSVParser
  ( RankingCSV
  , parseRankingCSV
  )
  where

import Prelude

import Data.String (Pattern(..))
import Data.String.Common (split, replace)
import Data.String.Pattern (Replacement(..))

type RankingCSV = Array (Array String)

normalizeAndSplitLines :: String -> Array String
normalizeAndSplitLines content =
  let
    normalizedContent = replace (Pattern "\r\n") (Replacement "\n") $
                        replace (Pattern "\r") (Replacement "\n") content
  in
    split (Pattern "\n") normalizedContent

parseRankingCSV :: String -> RankingCSV
parseRankingCSV content =
  let
    rows = normalizeAndSplitLines content
    cleanField = replace (Pattern "\r") (Replacement "")
    fields = map (\row -> map cleanField (split (Pattern ",") row)) rows
  in
    fields

