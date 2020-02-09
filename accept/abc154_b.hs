main :: IO ()
main = do
    s <- getLine
    putStrLn $ replicate (length s) 'x'
