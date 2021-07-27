import Data.List
import Data.Ord

takePairs []  = []
takePairs [_] = []
takePairs (a:b:xs)
  | a == b = a : takePairs xs
  | otherwise = takePairs (b:xs)

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    let a' = takePairs . concat
           . filter ((2<=) . length) . group . sortOn Down $ a
    print $ if length a' < 2
               then 0 else product $ take 2 a'
