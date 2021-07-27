main :: IO ()
main = do
    a <- read <$> getLine
    [b, c] <- map read . words <$> getLine
    s <- getLine
    putStrLn $ show (a + b + c) ++ " " ++ s
