import           Control.Monad
main = do
    [k,x] <- map read .words <$> getLine :: IO [Int]
    let i = k-1
        s = x-i
        e = x+i
    mapM_ print [s..e]
