main :: IO ()
main = do
    [a,b,x] <- map read . words <$> getLine :: IO [Double]
    print $
        if a * a * b / 2 <= x
            then let y = b - (x * 2 / (a * a) - b)
                  in (atan (y / a) * 180 / pi)
            else let y = 2 * x / b / a
                  in 90 - (atan (y / b) * 180 / pi)
