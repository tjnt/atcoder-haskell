import Data.List

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    ll <- map read . words <$> getLine :: IO [Int]
    print . sum . take k . reverse $ sort ll
