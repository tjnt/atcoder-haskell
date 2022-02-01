main :: IO ()
main = do
    s <- getLine :: IO String
    let h = head s
        l = last s
        m = show . length . tail $ init s
    putStrLn $ h : m ++ [l]
