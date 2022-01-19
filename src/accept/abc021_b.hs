import qualified Data.Set as S

main :: IO ()
main = do
    _ <- getLine
    [a,b] <- map read . words <$> getLine :: IO [Int]
    _ <- getLine
    pp <- map read . words <$> getLine :: IO [Int]
    let xs = (a : pp) <> [b]
        s  = S.fromList xs
    putStrLn $ if length xs == S.size s then "YES" else "NO"
