import Control.Monad
main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    a <- replicateM n $ drop 1 . map read . words <$> getLine :: IO [[Int]]
    print $ length [ i | i <- [1..m], all (\ai -> i `elem` ai) a ]
