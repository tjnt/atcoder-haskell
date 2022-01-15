import Data.Bits

main :: IO ()
main = do
    [n,x] <- map read . words <$> getLine :: IO [Int]
    a <- map read . words <$> getLine :: IO [Int]
    let xs = zip [0..] a :: [(Int,Int)]
    print $ sum [ p | (i,p) <- xs, testBit x i]
