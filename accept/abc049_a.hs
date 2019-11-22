main = getLine >>= (\c -> putStrLn $ if c `elem` ["a", "e", "i", "o", "u"] then "vowel" else "consonant")
