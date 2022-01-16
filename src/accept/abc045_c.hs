splitcmb :: String -> [(String,String)]
splitcmb s = [ splitAt i s | i <- [0..(length s - 1)] ]

splitcmbN :: String -> [[String]]
splitcmbN s = concatMap go $ splitcmb s
  where
    go :: (String,String) -> [[String]]
    go (s1,[]) = [[s1]]
    go ([],s2) = [[s2]]
    go (s1,s2) = map (s1 :) $ splitcmbN s2

solve :: String -> Int
solve s = sum . map (sum . map read) $ splitcmbN s

main :: IO ()
main = do
    s <- getLine :: IO String
    print $ solve s
