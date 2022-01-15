solve :: String -> String -> String
solve [] _      = []
solve (x:xs) ys = x : solve ys xs

main :: IO ()
main = do
    o <- getLine :: IO String
    e <- getLine :: IO String
    putStrLn $ solve o e
