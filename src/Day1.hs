module Day1 (day1) where

import Import
import Util
import qualified RIO.Text as T


parseInputLine :: Text -> Maybe Int
parseInputLine = readMaybe . T.unpack

day1 :: RIO App ()
day1 = processDay 1 (parseByLine parseInputLine) (display . largerCount) (display . largerCount . rollingAverage3)


largerCount :: [Int] -> Int
largerCount [] = 0
largerCount [_] = 0
largerCount (x1:x2:xs)
  | x1 < x2 = 1 + largerCount (x2:xs)
  | otherwise = largerCount (x2:xs)

rollingAverage3 :: [Int] -> [Int]
rollingAverage3 (x1:x2:x3:xs) = (x1 + x2 + x3) : rollingAverage3 (x2:x3:xs)
rollingAverage3 _ = []
