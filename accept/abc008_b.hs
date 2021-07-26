import Data.Map
import Data.List
import Data.Ord

main :: IO ()
main = do
    _ <- readLn :: IO Int
    s <- lines <$> getContents :: IO [String]
    let m = fromListWith (+) $ zip s (repeat 1)
    putStrLn $ fst . maximumBy (comparing snd) $ toList m
