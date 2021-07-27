import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.IArray
import Data.List
import qualified Data.IntSet as S

newtype UnionFind s = UnionFind (STArray s Int Int)

newUnionFind :: (Int,Int) -> [Int] -> ST s (UnionFind s)
newUnionFind ix li = UnionFind <$> newListArray ix li

root :: UnionFind s -> Int -> ST s Int
root (UnionFind a) x = do
    p <- readArray a x
    if p == x then return p
              else do rp <- root (UnionFind a) p
                      writeArray a x rp
                      return rp

same :: UnionFind s -> Int -> Int -> ST s Bool
same u x y = (==) <$> root u x <*> root u y

unite :: UnionFind s -> Int -> Int -> ST s ()
unite u@(UnionFind a) x y = do
    rx <- root u x
    ry <- root u y
    when (rx /= ry) $ do
        writeArray a rx ry

solve :: Int -> [(Int,Int)] -> Int
solve n xs = runST $ do
    u <- newUnionFind (1,n) [1..n] 
    go u xs
    pred . S.size . S.fromList <$> mapM (root u) [1..n]
  where
    go u [] = return ()
    go u ((a,b):xs) = do
        unite u a b
        go u xs

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    xs    <- map ((\[a,b] -> (a,b)) . (map read . words))
           . lines <$> getContents :: IO [(Int,Int)]
    print $ solve n xs
