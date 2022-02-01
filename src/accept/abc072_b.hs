main :: IO ()
main = do
    s <- getLine :: IO String
    putStrLn .  map snd .  filter (odd . fst) $ zip [1..] s
