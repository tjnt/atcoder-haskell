main :: IO ()
main = do
    _ <- getLine
    xy <- map ((\[a,b] -> (a,b)) . (map read . words))
        . lines <$> getContents :: IO [(Double,Double)]
    print $ maximum [ sqrt (((ix - jx) ^ 2) + ((iy - jy) ^ 2))
                    | (ix,iy) <- xy, (jx,jy) <- xy ]
