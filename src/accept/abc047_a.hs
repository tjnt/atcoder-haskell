import           Data.List
main = do
    a <- reverse . sort . map read . words <$> getLine
    putStrLn $ if head a == sum (drop 1 a) then "Yes" else "No"
