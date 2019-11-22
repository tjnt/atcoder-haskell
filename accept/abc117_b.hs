import           Data.List
main = do
    _ <- getLine
    lx <- sort . map read . words <$> getLine
    putStrLn $ if (last lx) < (sum . init $ lx) then "Yes" else "No"
