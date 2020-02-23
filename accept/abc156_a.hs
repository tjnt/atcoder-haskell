main :: IO ()
main = do
    [n,r] <- map read . words <$> getLine :: IO [Int]
    print $
        if n >= 10 then r
                   else r + 100 * (10 - n)
