module Day6 (day6) where

import Import
import Util
import qualified RIO.List as L
import qualified RIO.List.Partial as LP ((!!))
import qualified RIO.Map as Map
import qualified RIO.Text as T

day6 :: RIO App ()
day6 = processDay 6 parseCommaSepList part1 part2


countFish :: Int -> [Int] -> Utf8Builder
countFish n xs = display $ Map.foldl' (+) (0::Int) $ (L.iterate f start) LP.!! n
  where start = foldl' (\ m n' -> Map.insertWith (+) n' 1 m) Map.empty xs
        f m = foldl' g (Map.fromList [(6, cnt), (8, cnt)]) [1..8]
          where cnt = Map.findWithDefault 0 0 m
                g m' n' = Map.insertWith (+) (n'-1) (Map.findWithDefault 0 n' m) m'


part1 :: [Int] -> Utf8Builder
part1 = countFish 80


part2 :: [Int] -> Utf8Builder
part2 = countFish 256
