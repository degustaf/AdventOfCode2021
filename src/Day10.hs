module Day10 (day10) where

import Import
import Util
import qualified RIO.List as L
import qualified RIO.List.Partial as LP
import qualified RIO.Map as Map
import qualified RIO.Text as T


day10 :: RIO App ()
day10 = processDay 10 (parseByLine Just) part1 part2


matchingDelimiters :: Map.Map Char Char
matchingDelimiters = Map.fromList [('(',')'), ('[',']'), ('{','}'), ('<','>')]


processChunks :: Text -> Either Char Text 
processChunks input = case T.uncons input of
    Nothing -> Right ""
    Just (c, t) -> case matchingDelimiters Map.!? c of
      Nothing -> Left c
      Just delim -> case loop delim (t, "") of
        Right ("", ret) -> Right ret
        Right (t', _) -> processChunks t'
        Left r -> Left r
  where loop c' (src, ret) = case T.uncons src of
          Nothing -> Right (src, T.snoc ret c')
          Just (c'', src')
            | c' == c'' -> Right (src', ret)
            | otherwise -> case matchingDelimiters Map.!? c'' of
                Nothing -> Left c''
                Just delim' -> (loop delim' (src', ret)) >>= (loop c')


median :: Integral a => [a] -> a
median xs = (L.sort xs) LP.!! ((length xs) `div` 2)
-- We are explicitly told that this will only be used on odd lists.


part1 :: [Text] -> Int
part1 xs = sum $ map (f . processChunks) xs
  where f (Right _) = 0
        f (Left ')') = 3
        f (Left ']') = 57
        f (Left '}') = 1197
        f (Left '>') = 25137
        f (Left _) = 0


part2 :: [Text] -> Int
part2 = median . (map g) . (foldl' f []) . (map processChunks)
  where f xs (Right t) = t:xs
        f xs (Left _) = xs
        g = (foldl' h 0) . T.unpack
          where scores = Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]
                h acc c = case scores Map.!? c of
                  Nothing -> 5 * acc
                  Just n -> 5 * acc + n
