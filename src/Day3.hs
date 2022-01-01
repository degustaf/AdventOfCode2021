module Day3 (day3) where

import Import hiding (Down)
import Util
import qualified RIO.List as L
import qualified RIO.Text as T

data Binary = Zero | One deriving (Eq, Ord)

day3 :: RIO App ()
day3 = processDay 3 (parseByLine parseInputLine) part1 part2

parseInputLine :: Text -> Maybe [Binary]
parseInputLine line = sequence $ T.foldr (\c xs -> (f c) : xs) [] line
  where f '0' = Just Zero
        f '1' = Just One
        f _ = Nothing


part1 :: [[Binary]] -> Utf8Builder
part1 xs = display $ gammaToTotal (toInt b) (length b)
  where b = map occursMost $ L.transpose xs
        gammaToTotal n digits = n * (2^digits - 1 - n)


occursP :: (Int -> Int -> Bool) -> [Binary] -> Binary
occursP p xs = f 0 $ L.sort xs
  where f n [] = if n `p` 0 then Zero else One
        f n (Zero:bs) = f (n + 1) bs
        f n bs@(One:_) = if n `p` (length bs) then Zero else One

occursMost :: [Binary] -> Binary
occursMost = occursP (>)

occursLeast :: [Binary] -> Binary
occursLeast = occursP (<=)


toInt :: [Binary] -> Int
toInt = foldl' f 0
  where f n Zero = 2 * n
        f n One = 2 * n + 1


systemFilter :: ([Binary] -> Binary) -> [Binary] -> [[Binary]] -> [Binary]
systemFilter _ ret [] = ret
systemFilter _ ret [x] = ret ++ x
systemFilter p ret xs = systemFilter p (ret ++ [b]) $ map tail $ filter f xs
  where b = case L.transpose xs of
          (x:_) -> p x
          [] -> error "Unreachable"  -- The way that xs is constructed recursively, this should never be reached.
        f (a:_) = a == b
        f _ = False
        tail [] = []
        tail (_:ys) = ys


part2 :: [[Binary]] -> Utf8Builder
part2 xs = display $ oxygen * co2
  where oxygen = toInt $ lifeSupportFilter xs
        co2 = toInt $ co2Filter xs
        lifeSupportFilter = systemFilter occursMost []
        co2Filter = systemFilter occursLeast []
