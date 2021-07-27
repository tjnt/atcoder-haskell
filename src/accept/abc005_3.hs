import Data.List
import qualified Data.Sequence as Q

solve :: Int -> [Int] -> [Int] -> Bool
solve t a = go (Q.fromList a)
  where
    go :: Q.Seq Int -> [Int] -> Bool
    go q [] = True
    go q (x:xs) =
        maybe False (\i -> go (Q.drop (i+1) q) xs)
              (Q.findIndexL (\i -> x-t <= i && i <= x) q)

main :: IO ()
main = do
    t <- readLn :: IO Int
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Int]
    _ <- getLine
    b <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if solve t a b then "yes" else "no"
