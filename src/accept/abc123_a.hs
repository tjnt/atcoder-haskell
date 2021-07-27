import           Control.Monad
main = do
    x <- replicateM 5 readLn :: IO [Int]
    k <- readLn :: IO Int
    putStrLn $ if abs (maximum x - minimum x) <= k then "Yay!" else ":("
