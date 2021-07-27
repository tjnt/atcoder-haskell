main = do
    n <- readLn :: IO Int
    print . length $ [ n | n <- [1..n], odd (length (show n)) ]
