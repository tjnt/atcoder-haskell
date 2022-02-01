import Data.List

main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    print $ length . filter revEq $ [a..b]
  where
    revEq n = let s = show n
               in s == reverse s
