import qualified Data.Set as S

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    let a' = S.fromList a
    putStrLn $ if S.size a' == n then "YES" else "NO"
