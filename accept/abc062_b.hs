import Control.Monad
main :: IO ()
main = do
    [_,w] <- map read . words <$> getLine :: IO [Int]
    a <- lines <$> getContents :: IO [String]
    putStrLn $ replicate (w+2) '#'
    forM_ a $ \i -> do
        putChar '#'
        putStr i
        putStrLn "#"
    putStrLn $ replicate (w+2) '#'
