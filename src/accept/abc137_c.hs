import           Control.Monad
import           Data.List

main = do
    n <- readLn :: IO Int
    s <- sort . map sort <$> replicateM n getLine :: IO [String]
    print $ (sum . map (\i -> i * (i - 1) `div` 2) . map (fromIntegral . length)
            $ group s :: Integer)
