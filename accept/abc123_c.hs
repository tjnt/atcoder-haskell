ceil :: Integral a => a -> a -> a
ceil t s = (t + s - 1) `div` s

main :: IO ()
main = do
    (n:xs)  <- map read . lines <$> getContents :: IO [Integer]
    print $ ceil n (minimum xs) + 4
