main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ if odd n then n * ((n - 1) `div` 2)
                     else n * ((n - 1) `div` 2) + (n `div` 2)
