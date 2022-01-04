module Queue (
  Queue
 ,empty
 ,singleton
 ,fromList

 ,null
 ,head
 ,tail
 ,enqueue
 ,fold
 ,floodFill
) where

import Import hiding (fold, null)
import qualified RIO.Set as Set

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

singleton :: a -> Queue a
singleton a = Queue [a] []

fromList :: [a] -> Queue a
fromList a = Queue a []

null :: Queue a -> Bool
null (Queue [] []) = True
null _ = False

head :: Queue a -> Maybe a
head (Queue [] []) = Nothing
head (Queue [] back) = head $ Queue (reverse back) []
head (Queue (x:_) _) = Just x

tail :: Queue a -> Queue a
tail q | null q = empty
tail (Queue [] back) = tail $ Queue (reverse back) []
tail (Queue (_:xs) back) = Queue xs back

enqueue :: Queue a -> a -> Queue a
enqueue (Queue front back) a = Queue front (a:back)

fold :: (b -> a -> Queue a -> (b, Queue a)) -> b -> Queue a -> b
fold f b q = case head q of
  Nothing -> b
  Just a -> fold f b' q'
    where (b', q') = f b a (tail q)

floodFill :: (Ord a, Foldable t) => (a -> Bool) -> (a -> t a) -> a -> Set.Set a
floodFill inside adjacent = (fold f Set.empty) . singleton
  where f s pt q
          | inside pt && not (Set.member pt s) = (s', q')
            where s' = Set.insert pt s
                  q' = foldr (flip enqueue) q $ adjacent pt
        f s _ q = (s, q)
