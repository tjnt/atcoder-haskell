main :: IO ()
main = do
    x <- readLn :: IO Integer
    let expo = [ b^p | b <- [1..x], p <- [2..10], (b^p) <= x ]
    print . maximum $ (1:expo)
