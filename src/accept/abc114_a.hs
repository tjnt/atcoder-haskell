main = readLn >>= putStrLn . (\x -> if x `elem` [7,5,3] then "YES" else "NO")
