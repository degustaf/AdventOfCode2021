module Day11 (day11) where

import Import
import Util
import qualified Queue as Q
import qualified RIO.Char.Partial as C'
import qualified RIO.List as L
import qualified RIO.List.Partial as LP
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Vector as V
import qualified RIO.Vector.Partial as V'


day11 :: RIO App ()
day11 = processDay 11 (parseInput >=> validateInput) part1 part2


validateInput :: Vector (Vector Int) -> Maybe (Vector (Vector Int))
validateInput v
  | f v = Nothing
  | any f v = Nothing
  | any g v = Nothing
  | otherwise = Just v
    where f = (/= 10) . length
          g = any (\y -> y > 9 || y < 0)


parseInput :: Text -> Maybe (Vector (Vector Int))
parseInput lines_ = V.fromList <$> parseByLine parseInputLine lines_
  where parseInputLine txt = V.fromList <$> sequence (map f $ T.unpack txt)
        f c
          | c `elem` ['0'..'9'] = Just $ C'.digitToInt c
          | otherwise = Nothing


(!?) :: Vector(Vector a) -> (Int, Int) -> Maybe a
(!?) v (x, y) = do
  v' <- v V.!? y
  v' V.!? x


(//) :: Vector(Vector a) -> (Int, Int) -> a -> Vector(Vector a)
(//) v (x,y) a = v V'.// [(y, v')]
  where v' = (v V'.! y) V'.// [(x,a)]


update :: Vector(Vector a) -> (a -> a) -> (Int,Int) -> Vector(Vector a)
update v f x = case v!?x of
  Nothing -> v
  Just a -> (//) v x $ f a


directions :: [(Int, Int)]
directions = [(0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1)]


step :: Vector (Vector Int) -> (Vector (Vector Int), Set.Set (Int, Int))
step v = (V.map (V.map reset) v'', s')
  where v' = V.map (V.map (+1)) v
        g Nothing = False
        g (Just x) = x > 9
        q = Q.fromList [(x,y) | x<-[0..9], y<-[0..9], g $ v'!?(x,y)]
        reset n
          | n > 9 = 0
          | otherwise = n
        f (w,s) z@(x,y) q'
          | Set.member z s = ((w,s), q')
          | otherwise = ((w', Set.insert z s), foldl' Q.enqueue q' $ filter (g . (w'!?)) l)
            where inSquare (x',y') = 0<=x' && x'<=9 && 0<=y' && y'<=9
                  l = filter inSquare $ map (\(dx,dy) -> (x+dx,y+dy)) directions
                  w' = foldl' (\p -> update p (+1)) w l
        (v'', s') = Q.fold f (v', Set.empty) q


part1 :: Vector (Vector Int) -> Int
part1 w = snd $ (L.iterate f (w, 0)) LP.!! 100
  where f (v, n) = (v', n + length s)
          where (v', s) = step v


part2 :: Vector (Vector Int) -> Int
part2 = f 0
  where f n v
          | all (all (==0)) v = n
          | otherwise = f (n+1) $ fst $ step v
