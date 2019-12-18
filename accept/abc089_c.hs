import Data.List

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs' ]

main :: IO ()
main = do
    _ <- getLine
    s <- map head . lines <$> getContents
    let ns = [ (length . filter (==c)) s | c <- "MARCH"]
    print . sum . map product $ combinations 3 ns
