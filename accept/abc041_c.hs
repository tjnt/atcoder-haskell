import Data.List
import Data.Ord

main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Int]
    (mapM_ (print . fst) . sortOn (Down . snd)) $ zip [1..] a
