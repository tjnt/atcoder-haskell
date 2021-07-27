splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn t xs =
    case break (==t) xs of
        (a,[])  -> [a]
        (a,[_]) -> a:[[]]
        (a,_:b) -> a : splitOn t b

solve :: [Int] -> Int
solve = sum . map f . splitOn 0
  where
    f [] = 0
    f h = let m = minimum h
           in m + solve (map (+(-m)) h)

main :: IO ()
main = do
    n <- readLn :: IO Int
    h <- map read . words <$> getLine :: IO [Int]
    print $ solve h
