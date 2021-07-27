combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations n (x:xs) = [x:y | y <- combinations (n - 1) xs] ++ combinations n xs

main :: IO ()
main = do
    n <- readLn :: IO Int
    d <- map read . words <$> getLine :: IO [Int]
    let cmb = combinations 2 d
    print . sum $ [a * b | [a,b] <- cmb]
