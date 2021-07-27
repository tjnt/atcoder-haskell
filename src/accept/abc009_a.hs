main :: IO ()
main = readLn >>= \n -> print $ n `div` 2 + n `rem` 2
