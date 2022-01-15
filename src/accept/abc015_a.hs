main :: IO ()
main = do
    a <- getLine :: IO String
    b <- getLine :: IO String
    putStrLn $ if length a >= length b then a else b
