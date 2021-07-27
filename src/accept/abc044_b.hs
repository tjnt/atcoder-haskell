import           Data.List
main = do
    w <- getLine
    putStrLn $ if all even . map (`count` w) $ nub w then "Yes" else "No"
  where
    count c = length . filter (c==)
