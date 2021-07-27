alternateJoin :: [a] -> [a] -> [a]
alternateJoin [] _      = []
alternateJoin (x:xs) ys = x : alternateJoin ys xs

main :: IO ()
main = do
    _ <- getLine
    [s,t] <- words <$> getLine :: IO [String]
    putStrLn $ alternateJoin s t
