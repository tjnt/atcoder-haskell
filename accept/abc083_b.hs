import           Data.Char
main :: IO ()
main = do
    [n, a, b] <- map read . words <$> getLine
    let sum' = sum . map digitToInt . show
    print . sum . filter (\x -> a <= sum' x && sum' x <= b) $ [1..n]
