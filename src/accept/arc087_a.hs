import Data.List

f xs = case length xs of
         n | h == n -> 0
           | h < n  -> n - h
           | h > n  -> n
  where
    h = head xs

main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Int]
    print . sum . map f . group  . sort $ a
