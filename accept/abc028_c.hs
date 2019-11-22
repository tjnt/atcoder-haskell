import Data.List
import Data.Ord

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs' ]

main :: IO ()
main = do
    xs <- map read . words <$> getLine :: IO [Int]
    print $ (!!2) . sortOn Down . map sum . combinations 3 $ xs
