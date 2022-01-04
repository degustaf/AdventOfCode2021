module Util
  ( processDay
   ,parseByLine
   ,parseCommaSepList
   ,partUnimplemented
  ) where

import Import
import qualified RIO.ByteString as B
import qualified RIO.Text as T


parseByLine :: (Text -> Maybe a) -> Text -> Maybe [a]
parseByLine parseInputLine data_ = sequence $ map parseInputLine $ T.lines data_


parseCommaSepList :: Read a => Text -> Maybe [a]
parseCommaSepList line = sequence $ map (readMaybe . T.unpack) $ T.split (==',') line


processDay :: (Display b, Display c) => Int -> (Text -> Maybe a) -> (a -> b) -> (a -> c) -> RIO App ()
processDay n parseInput part1 part2 = do
  processDay' "SampleDay" n parseInput part1 part2
  processDay' "Day" n parseInput part1 part2


processDay' :: (Display b, Display c) => Text -> Int -> (Text -> Maybe a) -> (a -> b) -> (a -> c) -> RIO App ()
processDay' fileName n parseInput part1 part2 = do
  input <- B.readFile $ "input/" ++ (T.unpack fileName) ++ (show n) ++ ".txt"
  case T.decodeUtf8' input of
    Left err -> throwIO err
    Right data_ -> case parseInput data_ of
                     Nothing -> logError "Unable to parse input."
                     Just xs -> do
                                  logInfo ((display fileName) <> " " <> (display n) <> " part 1: " <> (display $ part1 xs))
                                  logInfo ((display fileName) <> " " <> (display n) <> " part 2: " <> (display $ part2 xs))


partUnimplemented :: a -> Text
partUnimplemented _ = "Not yet!"
