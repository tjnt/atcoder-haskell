solve a b k l = let (x,y) = k `divMod` l
                 in min (b * x + a * y) (b * (x+1))
main :: IO ()
main = do
    [a,b,k,l] <- map read . words <$> getLine :: IO [Int]
    print $ solve a b k l
