import Data.List

solve :: [Int] -> [Int]
solve aa = init $ x1 : f x1 aa
  where
    aa' = zip [(1::Int)..] aa
    x1 = sum [ a | (i,a) <- aa', odd i]
       - sum [ a | (i,a) <- aa', even i]
    f x [] = []
    f x (a:aa) = let x' = 2 * a - x
                  in x' : f x' aa

main :: IO ()
main = do
    _ <- getLine
    aa <- map read . words <$> getLine :: IO [Int]
    mapM_ print $ solve aa
