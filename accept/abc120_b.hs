import Data.List

main :: IO ()
main = do
    [a,b,k] <- map read . words <$> getLine :: IO [Int]
    let g = gcd a b
        c = [ i | i <- [1..g], a `mod` i == 0 && b `mod` i == 0]
    print $ c !! (length c - k)
