main :: IO ()
main = do
    h1 <- readLn :: IO Int
    h2 <- readLn :: IO Int
    print $ h1 - h2
