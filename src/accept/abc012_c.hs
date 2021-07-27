import Text.Printf
import Control.Monad

main :: IO ()
main = do
    n <- readLn :: IO Int
    let m = sum [ a * b | a <- [1..9], b <- [1..9] ] - n
    let res = [ (a,b) | a <- [1..9], b <- [1..9], a * b == m ]
    forM_ res $ \(a,b) -> do
        printf "%d x %d\n" a b
