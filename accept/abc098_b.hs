import Data.List

solve s i = let (a,b) = splitAt i s
             in length $ intersect (nub a) (nub b)

main :: IO ()
main = do
    n <- readLn :: IO Int
    s <- getLine
    print . maximum $ map (solve s) [0..(n-1)]
