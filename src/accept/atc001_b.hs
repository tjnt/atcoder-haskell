import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

newtype UnionFind s = UnionFind (MV.STVector s Int)

construct :: [Int] -> ST s (UnionFind s)
construct xs = UnionFind <$> (V.thaw . V.fromList) xs

root :: UnionFind s -> Int -> ST s Int
root (UnionFind v) x = do
    p <- MV.read v x
    if p == x then return p
              else do rp <- root (UnionFind v) p
                      MV.write v x rp
                      return rp

same :: UnionFind s -> Int -> Int -> ST s Bool
same u x y = (==) <$> root u x <*> root u y

unite :: UnionFind s -> Int -> Int -> ST s ()
unite u@(UnionFind v) x y = do
    rx <- root u x
    ry <- root u y
    when (rx /= ry) $ do
        MV.write v rx ry

solve :: Int -> [(Int,Int,Int)] -> [Bool]
solve n xs = runST $ do
    u <- construct [0..n]
    reverse <$> go u [] xs
  where
    go u r [] = return r
    go u r ((p,a,b):xs)
      | p == 0 = unite u a b >> go u r xs
      | p == 1 = same u a b >>= \b -> go u (b:r) xs
      | otherwise = go u r xs 

main :: IO ()
main = do
    [n,q] <- map read . words <$> getLine :: IO [Int]
    pabs <- map ((\[p,a,b] -> (p,a,b)) . (map read . words))
          . lines <$> getContents :: IO [(Int,Int,Int)]
    mapM_ (\b -> putStrLn $ if b then "Yes" else "No") $ solve n pabs
