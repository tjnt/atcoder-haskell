main :: IO ()
main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    print $ [i |i <- [1,2,3], i /= a && i /=b] !! 0
