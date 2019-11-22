import Control.Monad

isLightOn :: [Int] -> Int -> [Int] -> Bool
isLightOn k p s =
    let chk = if p == 0 then even else odd
        cnt = length . filter (==1) $ map (\i -> s !! (i-1)) k
    in chk cnt

solve :: [[Int]] -> [Int] -> [[Int]] -> Int
solve kk pp ss = length . filter (==True) $ map isAllOn ss
  where
    isAllOn s = all (\(k,p) -> isLightOn k p s) (zip kk pp)

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    k <- replicateM m $ tail . map read . words <$> getLine :: IO [[Int]]
    p <- map read . words <$> getLine :: IO [Int]
    print $ solve k p (replicateM n [0,1])
