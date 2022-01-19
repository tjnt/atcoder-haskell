main :: IO ()
main = do
    [a,b] <- words <$> getLine :: IO [String]
    let n = read (a <> b) :: Int
    print $ n * 2
