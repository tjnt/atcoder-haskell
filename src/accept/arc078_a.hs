import Data.List

main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Int]
    let s = sum a
    print . minimum . map (\i -> abs (i - (s-i))) . init $ scanl1 (+) a
