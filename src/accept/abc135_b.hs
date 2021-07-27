import           Data.List
main = do
    n <- readLn :: IO Int
    p <- map read . words <$> getLine :: IO [Int]
    let p' = sort p
        cnt =  length $ filter (==False) $ zipWith (==) p p'
    putStrLn $ if cnt > 2 then "NO" else "YES"
