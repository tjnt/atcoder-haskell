main :: IO ()
main = getLine >>= putStrLn . (\s -> if s == "a" then "-1" else "a")
