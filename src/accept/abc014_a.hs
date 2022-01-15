main :: IO ()
main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    let m = a `mod` b
    print $ if m == 0 then 0 else b - m
