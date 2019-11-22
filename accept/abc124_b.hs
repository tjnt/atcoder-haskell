import           Data.List
main = do
    _ <- getLine
    h <- map read . words <$> getLine
    print . fst . foldl' f (0, 0) $ h
  where
    f (c, m) a = if m <= a then (c+1, a) else (c, m)
