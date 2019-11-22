main = readLn >>= print . (\n -> 800 * n - n `div` 15 * 200)
