module Day5 (day5) where

import Import
import Util
import qualified RIO.Text as T

data Binary = Zero | One deriving (Eq, Ord)
data Line = Line (Int, Int) (Int, Int)

day5 :: RIO App ()
day5 = processDay 5 (parseByLine parseInputLine) part1 part2

parseInputLine :: Text -> Maybe Line
parseInputLine line = case T.words line of
  [a, _, b] -> Line <$> (maybePt a) <*> (maybePt b)
  _ -> Nothing


maybePt :: Text -> Maybe (Int, Int)
maybePt x = case T.split (\c -> c == ',') x of
  [a, b] -> (,) <$> readMaybe (T.unpack a) <*> readMaybe (T.unpack b)
  _ -> Nothing


verticalFn :: Int -> Int -> Int -> [[Int]] -> [[Int]]
verticalFn x y1 y2 [] = (replicate y1 []) ++ (replicate (y2 - y1 + 1) (f_ x []))
verticalFn x  0  0 (l:ls) = (f_ x l) : ls
verticalFn x  0 y2 (l:ls) = (f_ x l) : (verticalFn x 0 (y2-1) ls)
verticalFn x y1 y2 (l:ls) = l : (verticalFn x (y1-1) (y2-1) ls)


f_ :: Int -> [Int] -> [Int]
f_ x [] = (replicate x 0) ++ [1]
f_ 0 (l:ls) = (l+1):ls
f_ x (l:ls) = l:(f_ (x-1) ls)


horizontalFn :: Int -> Int -> Int -> [[Int]] -> [[Int]]
horizontalFn 0 x1 x2 [] = [(replicate x1 0) ++ (replicate (x2 - x1 + 1) 1)]
horizontalFn y x1 x2 [] = [] : horizontalFn (y-1) x1 x2 []
horizontalFn 0 x1 x2 (l:ls) = (f x1 x2 l) : ls
  where f :: Int -> Int -> [Int] -> [Int]
        f x1' x2' [] = (replicate x1' 0) ++ (replicate (x2' - x1' + 1) 1)
        f 0   0   (l0:l') = (l0 + 1) : l'
        f 0   x2' (l0:l') = (l0 + 1) : (f 0 (x2'-1) l')
        f x1' x2' (l0:l') = l0 : (f (x1'-1) (x2'-1) l')
horizontalFn y x1 x2 (l:ls) = l : horizontalFn (y-1) x1 x2 ls


diagonalFn :: Int -> Int -> Int -> (Int -> Int) -> [[Int]] -> [[Int]]
diagonalFn  0  0 x _   []     = [f_ x []]
diagonalFn  0 y2 x inc []     = (f_ x []) : (diagonalFn 0 (y2-1) (inc x) inc [])
diagonalFn y1 y2 x inc []     = [] : (diagonalFn (y1-1) (y2-1) x inc [])
diagonalFn  0  0 x _   (l:ls) = (f_ x l) : ls
diagonalFn  0 y2 x inc (l:ls) = (f_ x l) : (diagonalFn 0 (y2-1) (inc x) inc ls)
diagonalFn y1 y2 x inc (l:ls) = l : (diagonalFn (y1-1) (y2-1) x inc ls)


direction :: Bool -> Int -> Int
direction True  = \x -> x + 1
direction False = \x -> x - 1


lineFn :: Bool -> Line -> [[Int]] -> [[Int]]
lineFn b (Line (x1,y1) (x2,y2))
  | x1 == x2 && y1 < y2 = verticalFn x1 y1 y2
  | x1 == x2 = verticalFn x1 y2 y1
  | y1 == y2 && x1 < x2 = horizontalFn y1 x1 x2
  | y1 == y2 = horizontalFn y1 x2 x1
  | b && y1 < y2 = diagonalFn y1 y2 x1 $ direction (x1 < x2)
  | b = diagonalFn y2 y1 x2 $ direction (x2 < x1)
  | otherwise = id


part1 :: [Line] -> Utf8Builder
part1 = display . (length . filter (>1) . concat) . foldl' (\ls g -> g ls) [] . map (lineFn False)


part2 :: [Line] -> Utf8Builder
part2 = display . (length . filter (>1) . concat) . foldl' (\ls g -> g ls) [] . map (lineFn True)
