main :: IO ()
main = do
    ss <- lines <$> getContents :: IO [String]
    print $ length [ s | s <- ss, 'r' `elem` s]
