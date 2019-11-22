import           Data.List
main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine
    print . calc 0 0 . sortOn negate $ a
  where
    calc n i (a:ax) = calc (if even i then n + a  else n - a) (i + 1) ax
    calc n _ []     = n
