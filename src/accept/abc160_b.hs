main :: IO ()
main = do
    x <- readLn :: IO Int
    print $ (x `div` 500 * 1000) + ((x `mod` 500) `div` 5 * 5)
