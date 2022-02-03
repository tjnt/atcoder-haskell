import Data.List

main :: IO ()
main = do
    _ <- getLine
    aa <- map read . words <$> getLine :: IO [Int]
    print $ maximum aa - minimum aa
