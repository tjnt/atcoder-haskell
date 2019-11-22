main :: IO ()
main = do
    n <- getLine :: IO String
    if n == "1"
       then putStrLn "Hello World"
       else do
           a <- readLn :: IO Int
           b <- readLn :: IO Int
           print $ a + b
