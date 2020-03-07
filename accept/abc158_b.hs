ceilDiv :: Integral a => a -> a -> a
ceilDiv t s = (t + s - 1) `div` s

main :: IO ()
main = do
    [n,a,b] <- map read . words <$> getLine :: IO [Int]
    print $ (n `div` (a+b)) * a + min (n `mod` (a+b)) a
