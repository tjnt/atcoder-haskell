main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ n `div` 2 - (if even n then 1 else 0)
