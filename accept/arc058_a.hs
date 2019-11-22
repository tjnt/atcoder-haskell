import           Control.Monad
main = do
    [n, k] <- map read . words <$> getLine :: IO [Int]
    d <- filter (/= ' ') <$> getLine
    let x = [ i | i <- ['0'..'9'], i `notElem` d ]
    let l = length $ show n :: Int
    print $ g n l x
  where
    f n l = filter (n<=) . map (read :: String -> Int) . replicateM l
    g n l x =
        minimum $ case f n l x of
            [] -> f n (l+1) x
            _  -> f n l x
