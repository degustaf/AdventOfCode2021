module Day8 (day8) where

import Import
import Util
import qualified RIO.List as L
import qualified RIO.List.Partial as LP
import qualified RIO.Set as Set
import qualified RIO.Text as T


setSequence :: Ord a => Set.Set (Maybe a) -> Maybe (Set.Set a)
setSequence = Set.foldl' f (Just Set.empty)
  where f :: Ord a => Maybe (Set.Set a) -> Maybe a -> Maybe (Set.Set a)
        f (Just s) (Just a) = Just $ Set.insert a s
        f _ _ = Nothing

data Segment = A | B | C | D | E | F | G deriving (Eq, Ord)
data Input = Input [Set.Set Segment]
data Output = Output (Set.Set Segment) (Set.Set Segment) (Set.Set Segment) (Set.Set Segment)
data Entry = Entry Input Output

asList :: Output -> [Set.Set Segment]
asList (Output x0 x1 x2 x3) = [x0, x1, x2, x3]


day8 :: RIO App ()
day8 = processDay 8 (parseByLine parseInputLine) part1 part2


parseSegment :: Char -> Maybe Segment
parseSegment 'a' = Just A
parseSegment 'b' = Just B
parseSegment 'c' = Just C
parseSegment 'd' = Just D
parseSegment 'e' = Just E
parseSegment 'f' = Just F
parseSegment 'g' = Just G
parseSegment _ = Nothing


parseInputLine :: Text -> Maybe Entry
parseInputLine line = case T.split (=='|') line of
  [in_, out_] -> case f out_ of
        Just [x0, x1, x2, x3] -> Entry <$> (Input <$> (f in_)) <*> Just (Output x0 x1 x2 x3)
        _ -> Nothing
    where f = sequence . (map g) . T.words
          g = setSequence . Set.fromList . (map parseSegment) . T.unpack
  _ -> Nothing


mapping :: Input -> Output -> Int
mapping (Input xs) = f
  where xs' = L.sortOn length xs
        f o = foldl' (\ acc n -> 10 * acc + (g n)) 0 $ asList o
        g :: Set.Set Segment -> Int
        g s
          | length s == 2 = 1
          | length s == 3 = 7
          | length s == 4 = 4
          | length s == 5 && Set.intersection s one == one = 3
          | length s == 5 && length (Set.intersection s four) == 2 = 2
          | length s == 5 = 5
          | length s == 6 && length (Set.intersection s one) == 1 = 6
          | length s == 6 && Set.intersection s four == four = 9
          | length s == 6 = 0
          | length s == 7 = 8
          | otherwise = -100000
        one = xs' LP.!! 0
        four = xs' LP.!! 2


part1 :: [Entry] -> Utf8Builder
part1 = display . sum . (map f)
  where f (Entry _ out_) = length $ filter g $ asList out_
        g s = elem (length s) [2, 3, 4, 7]


part2 :: [Entry] -> Utf8Builder
part2 = display . sum . (map (\(Entry i o) -> mapping i o))
