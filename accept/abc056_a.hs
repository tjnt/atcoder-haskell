main = do
    [a, b] <- words <$> getLine
    putStrLn $ if a == "D"
        then if b == "H" then "D" else "H"
        else b
