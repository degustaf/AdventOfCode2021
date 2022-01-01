module Day2 (day2) where

import Import hiding (Down)
import Util
import qualified RIO.Text as T

data Direction = Forward Int | Down Int | Up Int

day2 :: RIO App ()
day2 = processDay 2 (parseByLine parseInputLine) part1 part2

parseInputLine :: Text -> Maybe Direction
parseInputLine = f . T.words
  where f :: [Text] -> Maybe Direction
        f [d, n] = case readMaybe $ T.unpack n of
                     Nothing -> Nothing
                     Just n' -> g d n'
        f _ = Nothing
        g :: Text -> Int -> Maybe Direction
        g "forward" n = Just $ Forward n
        g "down" n = Just $ Down n
        g "up" n = Just $ Up n
        g _ _ = Nothing


part1 :: [Direction] -> Utf8Builder
part1 ds = display $ combine $ foldl' g (0, 0) ds
  where combine :: (Int, Int) -> Int
        combine (f,s) = f * s
        g :: (Int, Int) -> Direction -> (Int, Int)
        g (h, d) (Forward n) = (h + n, d)
        g (h, d) (Down n) = (h, d + n)
        g (h, d) (Up n) = (h, d - n)
        -- Since we are measuring depth, Down and Up are the opposite of what is expected.

part2 :: [Direction] ->Utf8Builder
part2 ds = display $ combine $ foldl' g (0, 0, 0) ds
  where combine :: (Int, Int, Int) -> Int
        combine (f,s,_) = f * s
        g :: (Int, Int, Int) -> Direction -> (Int, Int, Int)
        g (h, d, a) (Down n) = (h, d, a+n)
        g (h, d, a) (Up n) = (h, d, a-n)
        g (h, d, a) (Forward n) = (h + n, d + a * n, a)
