module Day7 (day7) where

import Import
import Util
import qualified RIO.List as L
import qualified RIO.List.Partial as LP ((!!))

day7 :: RIO App ()
day7 = processDay 7 parseCommaSepList part1 part2


fuelUsedLinear :: Int -> [Int] -> Int
fuelUsedLinear mean xs = sum $ map (\n -> abs (n - mean)) xs


sum1ToN :: Integral a => a -> a
sum1ToN n = n * (n + 1) `div` 2


fuelUsedQuad :: Int -> [Int] -> Int
fuelUsedQuad avg xs = sum $ map (\n -> sum1ToN $ abs (n - avg)) xs


part1 :: [Int] -> Utf8Builder
part1 xs = display $ fuelUsedLinear mean xs
  where mean = (L.sort xs) LP.!! ((length xs) `div` 2)

part2 :: [Int] -> Utf8Builder
part2 xs = display $ min (fuelUsedQuad (ceiling avg) xs) (fuelUsedQuad (floor avg) xs)
  where (avg :: Double) = fromIntegral (sum xs) / fromIntegral (length xs)
