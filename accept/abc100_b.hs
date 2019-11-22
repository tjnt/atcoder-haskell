import Data.Array.IArray

main :: IO ()
main = do
    [d,n] <- map read . words <$> getLine :: IO [Int]
    let arr = listArray (0,n-1) [ i | i <- [1..], f i == d ] :: Array Int Int
    print $ arr!(n-1)
  where
    f x | x `rem` 100 /= 0 = 0
        | otherwise = 1 + f (x `div` 100)
