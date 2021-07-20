
deg :: Int -> String
deg d
  | x < 1124 = "N"
  | x < 3375 = "NNE"
  | x < 5625 = "NE"
  | x < 7875 = "ENE"
  | x < 10125 = "E"
  | x < 12375 = "ESE"
  | x < 14625 = "SE"
  | x < 16875 = "SSE"
  | x < 19125 = "S"
  | x < 21375 = "SSW"
  | x < 23625 = "SW"
  | x < 25875 = "WSW"
  | x < 28125 = "W"
  | x < 30375 = "WNW"
  | x < 32625 = "NW"
  | x < 34875 = "NNW"
  | otherwise = "N"
  where
    x = d * 10

dis :: Int -> Int
dis w
  | x <=   2  = 0
  | x <=  15  = 1
  | x <=  33  = 2
  | x <=  54  = 3
  | x <=  79  = 4
  | x <= 107  = 5
  | x <= 138  = 6
  | x <= 171  = 7
  | x <= 207  = 8
  | x <= 244  = 9
  | x <= 284  = 10
  | x <= 326  = 11
  | otherwise = 12
  where
    halfup x = let (m,d) = properFraction x in if d < 0.5 then m else m + 1
    x = halfup $ realToFrac w / 60 * 10

main :: IO ()
main = do
    [d,w] <- map read . words <$> getLine :: IO [Int]
    let ds = dis w
    let dg = if ds == 0 then "C" else deg d
    putStrLn $ dg ++ " " ++ show ds
