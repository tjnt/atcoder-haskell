import           Control.Monad
main = do
    _ <- getLine
    v <- map read . words <$> getLine
    c <- map read . words <$> getLine
    let xy = zipWith (-) v c
    print . maximum . map sum . filterM (const [True, False]) $ xy
