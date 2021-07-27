main :: IO ()
main = do
    n <- readLn :: IO Float
    print $ sum [i * 10000 * (1 / n) | i <- [1..n]]
