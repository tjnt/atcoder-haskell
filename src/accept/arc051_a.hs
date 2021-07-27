import Control.Monad

solve :: [Double] -> [Double] -> [Bool]
solve [x1,y1,r] [x2,y2,x3,y3] = [red,blue]
  where
    [px1,px2,py1,py2] = [x1-r,x1+r,y1+r,y1-r]
    diff (x,y) = (x - x1)^2 + (y - y1)^2
    red  = not $ px1 >= x2 && px2 <= x3 && py1 <= y3 && py2 >= y2
    blue = not . all (r^2 >=) $ map diff [(x2,y2),(x3,y2),(x3,y2),(x3,y3)]

main :: IO ()
main = do
    p1 <- map read . words <$> getLine :: IO [Double]
    p2 <- map read . words <$> getLine :: IO [Double]
    forM_ (solve p1 p2) $ \b ->
        putStrLn $ if b then "YES" else "NO"
