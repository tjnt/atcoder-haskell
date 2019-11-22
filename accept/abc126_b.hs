main = getLine >>= putStrLn . f . (\(a, b) -> (read a, read b)) . splitAt 2
  where
    f :: (Int, Int) -> String
    f (a, b)
      | inMonth a && inMonth b =  "AMBIGUOUS"
      | not (inMonth a) && inMonth b =  "YYMM"
      | inMonth a && not (inMonth b) =  "MMYY"
      | otherwise = "NA"
    inMonth x = 1 <= x && x <= 12
