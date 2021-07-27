main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ if even n then n `div` 2 else n `div` 2 + 1
