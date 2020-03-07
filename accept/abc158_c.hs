main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Double]
    let res = [ i | i <- [1..10000]
              , let k = fromIntegral i :: Double
                 in floor (k * 0.08) == floor a 
                 && floor (k * 0.1 ) == floor b ]
    print $ case res of
                [] -> -1
                xs -> minimum xs
