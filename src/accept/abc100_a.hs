main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if a <= 8 && b <= 8 then "Yay!" else ":("
