main :: IO ()
main = do
    _ <- getLine
    [t,a] <- map read . words <$> getLine :: IO [Double]
    h <- map read . words <$> getLine :: IO [Double]
    print . snd . minimum
          $ zip [ abs (t - a - x * 0.006) | x <- h ] [1..]
