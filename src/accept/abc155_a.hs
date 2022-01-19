main :: IO ()
main = do
    [a,b,c] <- map read . words <$> getLine :: IO [Int]
    putStrLn $
        if (a == b && a /= c)
        || (a == c && a /= b)
        || (b == c && b /= a)
           then "Yes" else "No"
