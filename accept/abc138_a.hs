main :: IO ()
main = do
    a <- readLn :: IO Int
    s <- getLine :: IO String
    putStrLn $ if a >= 3200 then s else "red"
