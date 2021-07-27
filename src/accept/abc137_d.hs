import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.IntMap as M

data HeapPolicy = MinHeap | MaxHeap

data HeapT p a = Empty p
               | HeapT p Int a (HeapT p a) (HeapT p a)

type Heap a = HeapT HeapPolicy a

empty :: HeapPolicy -> Heap a
empty = Empty

rank :: Heap a -> Int
rank (Empty _)         = 0
rank (HeapT _ r _ _ _) = r

constract :: HeapPolicy -> a -> Heap a -> Heap a -> Heap a
constract p x a b
  | rank a >= rank b = HeapT p (rank b + 1) x a b
  | otherwise        = HeapT p (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h (Empty _) = h
merge (Empty _) h = h
merge h1@(HeapT MinHeap _ x l1 r1) h2@(HeapT MinHeap _ y l2 r2)
  | x < y     = constract MinHeap x l1 (merge r1 h2)
  | otherwise = constract MinHeap y l2 (merge r2 h1)
merge h1@(HeapT MaxHeap _ x l1 r1) h2@(HeapT MaxHeap _ y l2 r2)
  | x > y     = constract MaxHeap x l1 (merge r1 h2)
  | otherwise = constract MaxHeap y l2 (merge r2 h1)
merge _ _     = error "invalid pattern match!"

push :: Ord a => a -> Heap a -> Heap a
push x h@(Empty p)         = merge (HeapT p 1 x (Empty p) (Empty p)) h
push x h@(HeapT p _ _ _ _) = merge (HeapT p 1 x (Empty p) (Empty p)) h

pop :: Ord a => Heap a -> Heap a
pop (Empty _)         = error "empty heap!"
pop (HeapT _ _ _ a b) = merge a b

top :: Heap a -> a
top (Empty _)         = error "empty heap!"
top (HeapT _ _ x _ _) = x

isEmpty :: Heap a -> Bool
isEmpty (Empty _) = True
isEmpty _         = False

mapFromList :: [(Int,Int)] -> M.IntMap [Int]
mapFromList [] = M.empty
mapFromList ((k,v):xs) =
    M.insertWith (\[a] b -> a:b) k [v] (mapFromList xs)

solve :: Int -> M.IntMap [Int] -> Int
solve m xs = f 1 (empty MaxHeap)
  where
    f :: Int -> Heap Int -> Int
    f i h
      | i > m = 0
      | otherwise =
          let h' = case M.lookup i xs of
                     Just ls  -> foldr push h ls
                     Nothing -> h
           in if isEmpty h'
                 then f (i+1) h'
                 else top h' + f (i+1) (pop h')

main :: IO ()
main = do
    [_,m] <- map read . words <$> getLine :: IO [Int]
    xs <- map ((\[a,b] -> (a,b))
        . map (fst . fromJust . BS.readInt) . BS.words)
        . BS.lines <$> BS.getContents
    print . solve m $ mapFromList xs
