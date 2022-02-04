import Text.Printf

main :: IO ()
main = do
    l <- readLn :: IO Double
    printf "%.8f" $ (l/3)^3
