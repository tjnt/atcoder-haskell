main :: IO ()
main = interact (yesno . (==0) . (`mod` 3) . read)
  where
    yesno b = if b then "YES" else "NO"
