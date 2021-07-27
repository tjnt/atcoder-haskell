import Data.List

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    r <- map read . words <$> getLine :: IO [Double]
    let r' = drop (n-k) $ sort r
    print $ foldl (\a x -> (a + x) / 2) 0 r'
