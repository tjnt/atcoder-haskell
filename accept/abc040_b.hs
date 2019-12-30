solve :: Int -> Int
solve n = minimum . map f $ [1..sq]
  where
    sq = truncate $ sqrt (fromIntegral n :: Float)
    f x = let (y,m) = n `divMod` x in abs (x - y) + m

main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ solve n
