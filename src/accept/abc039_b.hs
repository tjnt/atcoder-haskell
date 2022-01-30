main :: IO ()
main = (readLn :: IO Double) >>= print . truncate . sqrt . sqrt
