import Data.List

main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Int]
    let s = minimum a
        g = maximum a
     in print $ g - s
