import Data.List

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    h <- map read . words <$> getLine :: IO [Int]
    let h' =  reverse $ sort h
    print . sum $ drop k h'
