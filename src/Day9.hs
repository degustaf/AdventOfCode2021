module Day9 (day9) where

import Import
import Util
import Queue as Q
import qualified RIO.Char.Partial as C'
import qualified RIO.List as L
import qualified RIO.Text as T
import qualified RIO.Vector as V
import qualified RIO.Vector.Partial as V'


day9 :: RIO App ()
day9 = processDay 9 parseInput part1 part2


parseInput :: Text -> Maybe (Vector (Vector Int))
parseInput lines_ = V.fromList <$> parseByLine parseInputLine lines_
  where parseInputLine txt = V.fromList <$> sequence (map f $ T.unpack txt)
        f c
          | c `elem` ['0'..'9'] = Just $ C'.digitToInt c
          | otherwise = Nothing


(!?) :: Vector(Vector Int) -> (Int, Int) -> Maybe Int
(!?) v (x, y) = do
  v' <- v V.!? y
  v' V.!? x


(!) :: Vector(Vector Int) -> (Int, Int) -> Int
(!) v idx = case v !? idx of
  Nothing -> maxBound
  Just n -> n


directions :: [(Int, Int)]
directions = [(0,1), (0,-1), (1,0), (-1,0)]


local_mins :: Vector (Vector Int) -> [(Int, Int)]
local_mins v = filter local_min idxs
  where y_max = length v
        x_max = length (v V'.! 0)
        idxs = [(x, y) | x <- [0..x_max-1], y <- [0..y_max-1]]
        local_min pt = all (f pt) directions
        f (x,y) (dx,dy) = v!(x,y) < v!(x+dx,y+dy)
  

part1 :: Vector (Vector Int) -> Int
part1 v = sum $ map (\idx -> v!idx + 1) $ local_mins v

part2 :: Vector (Vector Int) -> Int
part2 v = case L.sortBy (flip compare) $ map f $ local_mins v of
    (x0:x1:x2:_) -> x0 * x1 * x2
    _ -> -1
  where f pt = length $ Q.floodFill (\x -> v!x < 9) adjacent pt
        adjacent (x,y) = map (\(x1,y1) -> (x+x1,y+y1)) directions
