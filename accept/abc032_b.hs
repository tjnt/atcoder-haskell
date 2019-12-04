import Data.List

takeN :: Int -> [a] -> [[a]]
takeN n [] = []
takeN n as@(_:xs) = take n as : takeN n xs

main :: IO ()
main = do
    s <- getLine
    k <- readLn :: IO Int
    print $ length . nub . filter ((==k) . length) $ takeN k s
