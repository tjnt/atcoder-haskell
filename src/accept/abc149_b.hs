import Data.Int

main :: IO ()
main = do
    [a,b,k] <- map read . words <$> getLine :: IO [Int64]
    let k' = max 0 (k - a)
        a' = max 0 (a - k)
        b' = max 0 (b - k')
    putStr $ show a'
    putStr " "
    putStrLn $ show b'
