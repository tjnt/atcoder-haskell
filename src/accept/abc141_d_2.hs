data Tree a = Empty
            | Fork Int a (Tree a) (Tree a)

newtype MinHeap a = MinHeap (Tree a)
newtype MaxHeap a = MaxHeap (Tree a)

type HeapPolicy a = (a -> a -> Bool)

class Heap a where
  policy :: Ord e => a e -> HeapPolicy e
  wrap :: Tree e -> a e
  unwrap :: a e -> Tree e

instance Heap MinHeap where
  policy _ = (<)
  wrap t = MinHeap t
  unwrap (MinHeap t) = t

instance Heap MaxHeap where
  policy _ = (>)
  wrap t = MaxHeap t
  unwrap (MaxHeap t) = t

empty :: Heap a => a e
empty = wrap Empty

push :: (Heap a, Ord e) => a e -> e -> a e
push h v = wrap
         $ merge (policy h) (Fork 1 v Empty Empty) (unwrap h)

pop :: (Heap a, Ord e) => a e -> a e
pop h = go $ unwrap h
  where
    go Empty          = error "empty heap!"
    go (Fork _ _ a b) = wrap (merge (policy h) a b)

top :: Heap a => a e -> e
top = go . unwrap
  where
    go Empty          = error "empty heap!"
    go (Fork _ x _ _) = x

isEmpty :: Heap a => a e -> Bool
isEmpty = go . unwrap
  where
    go Empty = True
    go _     = False

fromList :: (Heap a, Ord e) => [e] -> a e
fromList = foldr (flip push) empty

toList :: (Heap a, Ord e) => a e -> [e]
toList h
  | isEmpty h = []
  | otherwise = top h : toList (pop h) 

rank :: Tree a -> Int
rank Empty          = 0
rank (Fork r _ _ _) = r

join :: a -> Tree a -> Tree a -> Tree a
join x a b
  | rank a >= rank b = Fork (rank b + 1) x a b
  | otherwise        = Fork (rank a + 1) x b a

merge :: HeapPolicy a -> Tree a -> Tree a -> Tree a
merge _ t Empty = t
merge _ Empty t = t
merge p t1@(Fork _ x l1 r1) t2@(Fork _ y l2 r2)
  | p x y     = join x l1 (merge p r1 t2)
  | otherwise = join y l2 (merge p r2 t1)


solve :: Int -> [Int] -> Int
solve m xs = sum . toList $ go m h
  where
    h = fromList xs :: MaxHeap Int
    go 0 h = h
    go m h = let x  = top h `div` 2
                 h' = push (pop h) x
              in go (m-1) h'

main :: IO ()
main = do
    [n, m] <- map read . words <$> getLine :: IO [Int]
    xs <- map read . words <$> getLine :: IO [Int]
    print $ solve m xs
