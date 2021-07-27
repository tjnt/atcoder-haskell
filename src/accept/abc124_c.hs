main :: IO ()
main = do
    s <- getLine
    let n = length s
        a = take n $ cycle "01"
        b = take n $ cycle "10"
     in print $ n - max (cntEq s a) (cntEq s b)
  where
    cntEq :: String -> String -> Int
    cntEq x y = length . filter (uncurry (==)) $ zip x y
