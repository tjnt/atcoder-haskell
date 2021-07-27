import Data.List
import Data.Maybe

main :: IO ()
main = do
    _ <- getLine
    s <- getLine
    let le = scanl (\a c -> if c == 'E' then a+1 else a) 0 s
        rw = scanr (\c a -> if c == 'W' then a+1 else a) 0 s
        z = zipWith (+) le rw
        p = fromJust $ elemIndex (maximum z) z
        (pl,pr) = splitAt p s
        count f = length . filter f
    print $ count (=='W') pl + count (=='E') pr
