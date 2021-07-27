import Control.Monad
import Data.List

check :: [[Int]] -> [Int] -> Bool
check a b = any f a
  where
    f = all (`elem` b)

check2 :: [[Int]] -> [Int] -> Bool
check2 a b = any f a'
  where
    a' = let p = concat a
          in [[p!!0, p!!4, p!!8], [p!!2, p!!4, p!!6]]
    f xs = all (\x -> x `elem` b) xs

main :: IO ()
main = do
    a <- replicateM 3 $ map read . words <$> getLine :: IO [[Int]]
    n <- readLn :: IO Int
    b <- map read . lines <$> getContents :: IO [Int]
    putStrLn $
        if check a b || check (transpose a) b || check2 a b
           then "Yes" else "No"
