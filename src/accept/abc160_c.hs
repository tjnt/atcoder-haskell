import Data.List

main :: IO ()
main = do
    [k,n] <- map read . words <$> getLine :: IO [Int]
    xs <- map read . words <$> getLine :: IO [Int]
    let xs' = zipWith (-) (tail xs ++ [k + head xs]) xs
    print $ sum xs' - foldl' max 0 xs'
