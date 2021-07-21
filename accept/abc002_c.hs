main :: IO ()
main = do
    [ax,ay,bx,by,cx,cy] <- map read . words <$> getLine :: IO [Int]
    let (a,b) = (bx - ax, by - ay)
    let (c,d) = (cx - ax, cy - ay)
    print $ realToFrac (abs (a*d - b*c)) / 2
