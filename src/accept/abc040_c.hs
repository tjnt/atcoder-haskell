import Data.List (unfoldr)

diff :: Int -> Int -> Int
diff x y = abs (x - y)

solve :: [Int] -> Int
solve [x1,x2] = diff x1 x2
solve xs =
    let (x1:x2:_) = xs
     in last $ unfoldr f (0, diff x1 x2, xs)
  where
    f :: (Int,Int,[Int]) -> Maybe (Int, (Int,Int,[Int]))
    f (r1,r2,x1:x2:x3:xs) =
        let d1 = r1 + diff x1 x3
            d2 = r2 + diff x2 x3
            r3 = min d1 d2
         in Just (r3, (r2, r3, x2:x3:xs))
    f (_,_,xs) = Nothing

main :: IO ()
main = do
    _ <- getLine
    xs <- map read . words <$> getLine :: IO [Int]
    print $ solve xs
