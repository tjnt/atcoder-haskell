main = do
    [w, h, x, y] <- map read . words <$> getLine
    let a = w * h / 2 :: Double
    let b = if w / 2 == x && h / 2 == y then "1" else "0"
    putStrLn $ show a ++ " " ++ b
