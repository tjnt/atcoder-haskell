import Data.List
main :: IO ()
main = do
    _ <- getLine
    h1:h <- map read . words <$> getLine :: IO [Int]
    let (a,_,_) = foldl' f (0,0,h1) h
    print a
  where
    f (m,c,i) j =
        let c' = if i >= j then c+1 else 0
         in (max m c', c', j)
