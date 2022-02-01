main :: IO ()
main = do
    [a,b,c,d] <- map read . words <$> getLine :: IO [Int]
    print $ max 0 $ min b d - max a c 
