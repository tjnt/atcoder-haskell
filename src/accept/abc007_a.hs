main :: IO ()
main = (readLn :: IO Int) >>= print . pred
