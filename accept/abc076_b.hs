main :: IO ()
main = do
    n <- readLn :: IO Int
    k <- readLn :: IO Int
    let f x = min (x+k) (x*2)
    print $ iterate f 1 !! n
