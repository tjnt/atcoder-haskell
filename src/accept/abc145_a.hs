main :: IO ()
main = do
    r <- readLn :: IO Int
    print (r*r)
