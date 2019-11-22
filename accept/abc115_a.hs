main :: IO ()
main = do
    d <- readLn :: IO Int
    putStrLn $ case d of
        22 -> "Christmas Eve Eve Eve"
        23 -> "Christmas Eve Eve"
        24 -> "Christmas Eve"
        25 -> "Christmas"
