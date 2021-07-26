main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    let xs = [ [a,b,c] | b <- [0,1], a <-[0..n-b], let c = n-a-b,
               a * 2 + b * 3 + c * 4 == m ]
    putStrLn . unwords . map show $
        if null xs then [-1,-1,-1] else head xs
