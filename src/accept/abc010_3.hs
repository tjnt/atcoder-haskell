l2norm :: (Integral a, Floating b) => [a] -> [a] -> b
l2norm a b = sqrt . fromIntegral . sum $ zipWith (\i j -> (i - j) ^ 2) a b

solve :: [Int] -> [Int] -> Int -> Int -> [[Int]] -> Bool
solve ta tb t v xys = any (tv >=) (map f xys)
  where
    tv = fromIntegral $ t * v :: Double
    f xy = l2norm ta xy + l2norm xy tb

main :: IO ()
main = do
    [txa,tya,txb,tyb,t,v] <- map read . words <$> getLine :: IO [Int]
    _ <- getLine
    xy <- map (map read . words) . lines <$> getContents :: IO [[Int]]
    putStrLn $ if solve [txa,tya] [txb,tyb] t v xy then "YES" else "NO" 
