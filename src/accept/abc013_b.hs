main :: IO ()
main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    print $ minimum [abs (a-b), abs(a-(b+10)), abs(a+10-b)]
