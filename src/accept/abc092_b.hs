main :: IO ()
main = do
    _ <- getLine
    [d,x] <- map read . words <$> getLine :: IO [Int]
    aa <- map read . lines <$> getContents :: IO [Int]
    print . (+x) . sum . map length$ [[1,a+1..d] | a <- aa]
