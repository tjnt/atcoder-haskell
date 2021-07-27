main :: IO ()
main = do
    [sx,sy,tx,ty] <- map read . words <$> getLine :: IO [Int]
    let x = tx - sx
        y = ty - sy
        a = [ (y,'U'), (x,'R'), (y,'D'), (x+1,'L')
            , (y+1,'U'), (x+1,'R'), (1,'D'), (1,'R')
            , (y+1,'D'), (x+1,'L'), (1,'U')]
    putStrLn $ concatMap (uncurry replicate) a
