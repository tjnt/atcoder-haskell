import Control.Monad

main :: IO ()
main = do
    n <- readLn :: IO Int
    print . sum $ do
        i <- [1..n]
        guard $ odd i
        guard $ length [j | j <- [1..i], i `rem` j == 0] == 8
        return 1
