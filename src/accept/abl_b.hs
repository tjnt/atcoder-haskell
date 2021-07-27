main :: IO ()
main = do
    xs <- map read . words <$> getLine :: IO [Int]
    putStrLn $ case xs of
        [a,b,c,d] | a <= c && c <= b -> "Yes"
                  | c <= a && a <= d -> "Yes"
                  | otherwise -> "No"
