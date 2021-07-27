import Data.List

main :: IO ()
main = do
    n <- readLn :: IO Int
    v <- map read . words <$> getLine :: IO [Double]
    print $ foldl1' f $ sort v
  where
    f a b = (a + b) / 2
