isInteger :: RealFrac a => a -> Bool
isInteger x = x == fromInteger (round x)

main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    let k = abs (fromIntegral b + fromIntegral a) / 2
    putStrLn $
      if isInteger k then show $ round k else "IMPOSSIBLE"
