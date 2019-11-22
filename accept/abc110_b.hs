main :: IO ()
main = do
    [_,_,xx,yy] <- map read . words <$> getLine :: IO [Int]
    x <- map read . words <$> getLine :: IO [Int]
    y <- map read . words <$> getLine :: IO [Int]
    let mx = maximum x
        my = minimum y
        n  = length [ z | z <- [(xx+1)..yy], mx < z && my >= z]
    putStrLn $ if n == 0 then "War" else "No War"
