import Data.List
import Data.Array.IArray

solve :: [Int] -> Int
solve a = dp!(n,0)
  where
    n = length a
    aa = listArray (1,n) a :: Array Int Int
    k = ((0,0),(n,1))
    dp = listArray k $ map f (range k) :: Array (Int,Int) Int
    f (0,0) = 0
    f (0,1) = minBound `div` 2
    f (i,0) = max (dp!(i-1,0) + (aa!i)) (dp!(i-1,1) - (aa!i))
    f (i,1) = max (dp!(i-1,0) - (aa!i)) (dp!(i-1,1) + (aa!i))

main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Int]
    print $ solve a
