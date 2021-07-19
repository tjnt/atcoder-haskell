main :: IO ()
main = readLn >>= \n -> print $ if n == 12 then 1 else n+1
