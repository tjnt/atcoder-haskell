main :: IO ()
main = do
    [m,d] <- map read . words <$> getLine :: IO [Int]
    putStrLn $if m `mod` d == 0
                 then "YES" else "NO"
