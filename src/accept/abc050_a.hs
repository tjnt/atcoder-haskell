main = do
    [a, op, b] <- words <$> getLine
    print $ (if op == "+" then (+) else (-)) (read a) (read b)
