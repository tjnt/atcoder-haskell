import Data.List
main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    print $ if a `elem` [1..9] && b `elem` [1..9]
               then a * b else -1
