import           Data.List
main = do
    n <- readLn :: IO Int
    d <- sort . map read . words <$> getLine :: IO [Int]
    print $ (d !! (n `div` 2)) - (d !! ((n `div` 2) - 1))
