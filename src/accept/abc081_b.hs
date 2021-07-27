main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine
    let divide x n = if all even x
        then divide (map (`div` 2) x) (n + 1) else n
    print $ divide a 0
