import Data.Bits

main :: IO ()
main = do
    [_, x] <- map read . words <$> getLine :: IO [Int]
    a <- map read . words <$> getLine :: IO [Int]
    print . sum . map snd . filter (testBit x . fst)
       $ zip ([0..] :: [Int]) a
