main :: IO ()
main = do
    k <- readLn :: IO Int
    let o = filter odd  [1..k]
        e = filter even [1..k]
    print $ length o * length e
