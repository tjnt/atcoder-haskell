main :: IO ()
main = do
    t <- tail . map read . lines <$> getContents :: IO [Int]
    print $ minimum t
