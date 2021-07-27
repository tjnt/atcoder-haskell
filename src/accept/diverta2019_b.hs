main :: IO ()
main = do
    [r,g,b,n] <- map read . words <$> getLine :: IO [Int]
    print . sum
        $ [ 1
          | i <- [0..(n`div`r)]
          , j <- [0..((n-r*i)`div`g)]
          , let k = n-r*i-g*j in k `rem` b == 0
          ]
