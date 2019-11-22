import           Data.List
main = do
    s <- getLine :: IO String
    print . fst $ foldl f (0,0) s
  where
    f :: (Int,Int) -> Char -> (Int,Int)
    f (m,n) x = if x `elem` "ACGT"
                   then (max m (n+1), n+1) else (m, 0)
