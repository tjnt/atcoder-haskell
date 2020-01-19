main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    putStrLn . concatMap show
        $ if a < b then replicate b a else replicate a b
