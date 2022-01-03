module Day4 (day4) where

import Import hiding (Down)
import Util
import qualified RIO.List as L
import qualified RIO.Text as T

data Entry = Unmarked Int | Marked Int deriving Show
data Board = Board [[Entry]] deriving Show
data Puzzle = Puzzle [Int] [Board]

day4 :: RIO App ()
day4 = processDay 4 parseInput part1 part2

parseInput :: Text -> Maybe Puzzle
parseInput data_ = case T.lines data_ of
  (x:xs) -> Puzzle <$> (sequence draws) <*> (sequence bs)
    where (draws :: [Maybe Int]) = map (readMaybe . T.unpack) $ T.split (==',') x
          bs = parseBoards xs
  _ -> Nothing


parseBoards :: [Text] -> [Maybe Board]
parseBoards [] = []
parseBoards (x:xs)
  | T.null $ T.strip x = parseBoards xs
parseBoards (x0:x1:x2:x3:x4:xs) = (boardMaybe [x0, x1, x2, x3, x4]) : parseBoards xs
  where boardMaybe :: [Text] -> Maybe Board
        boardMaybe ts = Board <$> (sequence $ (map f) ts)
        f :: Text -> Maybe [Entry]
        f l = sequence $ map (\x -> Unmarked <$> readMaybe (T.unpack x)) $ T.words l
parseBoards _ = [Nothing]


draw :: Int -> Board -> Board
draw n (Board entries) = Board $ map (map f) entries
  where f (Unmarked n')
          | n' == n = Marked n'
        f x = x


isMarked :: Entry -> Bool
isMarked (Unmarked _) = False
isMarked (Marked _)   = True


winner :: Board -> Bool
winner (Board entries) = (f entries) || (f $ L.transpose entries)
  where f = any (all isMarked)


score :: Int -> Board -> Int
score n (Board entries) = n * (sum $ map f $ concat entries)
  where f :: Entry -> Int
        f (Unmarked n') = n'
        f _ = 0


part1 :: Puzzle -> Int
part1 (Puzzle (n:draws) boards) = case filter winner boards' of 
                                    [] -> part1 $ Puzzle draws boards'
                                    w:_ -> score n w
  where boards' = map (draw n) boards
part1 (Puzzle [] _) = 0


part2 :: Puzzle -> Int
part2 (Puzzle (n:draws) boards)
  | all winner boards' = case boards' of
                           [] -> 0
                           (w:_) -> score n w
  | otherwise = part2 $ Puzzle draws $ filter (not . winner) boards'
  where boards' = map (draw n) boards
part2 (Puzzle [] _) = 0
