import Data.List

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . lines <$> getContents :: IO [Int]
    let a' = nub . sort $ a
    print $ a' !! (length a' - 2)
