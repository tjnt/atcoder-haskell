main :: IO ()
main = do
    [a1,a2,a3] <- map read . words <$> getLine :: IO [Int]
    putStrLn $
        if a1 + a2 + a3 >= 22 then "bust" else "win"
