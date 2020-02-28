import Control.Monad
import Data.Array.IArray
import Data.Bits
import Data.Bool

bit2List :: Int -> Int -> [Int]
bit2List b n = go 0
  where
    go i | i  == n   = []
         | bi /= 0   = i : go (i+1)
         | otherwise = go (i+1)
      where bi = b .&. shift 1 i

solve :: Array Int [(Int,Int)] -> Int
solve a = maximum $ go1 (0 :: Int)
  where
    n = length a
    go1 b | b == shift 1 n = []
          | otherwise = go2 b : go1 (b+1)
    go2 b = let lb = bit2List b n
             in bool 0 (length lb) $ all f lb
      where
        f i = all g (a!i)
        g (x,y) = let p = b .&. shift 1 (x-1)
                   in ((y == 1 && p /= 0) || (y == 0 && p == 0))

main :: IO ()
main = do
    n <- readLn :: IO Int
    xy <- replicateM n $ do
        a <- readLn :: IO Int
        replicateM a $ do
            [x,y] <- map read . words <$> getLine :: IO [Int]
            return (x,y)
    let xy' = listArray (0,n-1) xy :: Array Int [(Int,Int)]
    print $ solve xy'
