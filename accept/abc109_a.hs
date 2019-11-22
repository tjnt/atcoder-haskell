main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if any odd $ map (\c -> a * b * c) [1..3]
                  then "Yes" else "No"
              
