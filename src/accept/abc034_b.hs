main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ if odd n then n+1 else n-1
