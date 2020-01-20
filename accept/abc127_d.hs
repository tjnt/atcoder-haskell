import           Control.Monad
import           Control.Monad.ST
import           Data.Array.IArray
import           Data.Array.ST
import           Data.Function
import           Data.Int
import           Data.List
import           Data.Ord

solve :: [Int] -> [(Int,Int)] -> Int64
solve a bc = runST $ thaw a' >>= \m -> go m 1 bc'
  where
    n = length a
    a' = listArray (1,n) $ sort a :: Array Int Int
    bc' = sortOn (Down . snd) bc
    go :: STArray s Int Int -> Int -> [(Int,Int)] -> ST s Int64
    go m p [] = sum . map fromIntegral <$> getElems m
    go m p ((b,c):xs)
      | n < p = go m p []
      | otherwise = do
        forM_ [p..(p+b-1)] $ \i ->
            when (i <= n) $ do
                mi <- readArray m i
                when (mi < c) $ writeArray m i c
        go m (p+b) xs

main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    a  <- map read . words <$> getLine :: IO [Int]
    bc <- map ((\[b,c] -> (b,c)) . (map read . words)) . lines
      <$> getContents :: IO [(Int,Int)]
    print $ solve a bc
