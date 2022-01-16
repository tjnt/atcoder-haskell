isInteger x = x == fromInteger (round x)

main :: IO ()
main = do
    [a,b] <- words <$> getLine :: IO [String]
    let x = read (a ++ b) :: Int
        s = sqrt (fromIntegral x) :: Double
    putStrLn $ if isInteger s then "Yes" else "No"
