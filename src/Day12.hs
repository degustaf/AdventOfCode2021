module Day12 (day12) where

import Import
import Util
-- import qualified Queue as Q
-- import qualified RIO.Char.Partial as C'
-- import qualified RIO.List as L
-- import qualified RIO.List.Partial as LP
-- import qualified RIO.Set as Set
import qualified RIO.Text as T
-- import qualified RIO.Vector as V
-- import qualified RIO.Vector.Partial as V'


day12 :: RIO App ()
day12 = processDay 12 (parseByLine parseInput) partUnimplemented partUnimplemented


parseInput :: Text -> Maybe (Text, Text)
parseInput t = case T.split (=='-') t of
  [t1,t2] -> Just (t1, t2)
  _ -> Nothing


part1 :: [(Text, Text)] -> Int
part1 = length


-- part2 :: Vector (Vector Int) -> Int
