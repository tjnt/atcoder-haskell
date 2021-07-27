main :: IO ()
main = do
    _ <- getLine
    t  <- map read . lines <$> getContents :: IO [Int]
    print $ foldr1 lcm t
