import Data.List

main :: IO ()
main = do
    xs <- map read . words <$> getLine :: IO [Int]
    k <- readLn :: IO Int
    let (x:xs') = reverse $ sort xs
    print . sum $ (x*2^k):xs'
