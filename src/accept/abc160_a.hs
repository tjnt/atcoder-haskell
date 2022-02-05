main :: IO ()
main = do
    s <- getLine :: IO String
    putStrLn $ if s!!2 == s!!3 && s!!4 == s!!5 then "Yes" else "No"
