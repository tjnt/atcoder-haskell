main :: IO ()
main = do
    s <- getLine
    putStrLn $
        if all (== head s) s then "No" else "Yes"
