import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Array.IArray
import Control.Monad

solve :: Array (Int,Int) Int -> [Int]
solve c = do
    a1 <- [0..100]
    let b1 = c!(1,1) - a1
        b2 = c!(1,2) - a1
        b3 = c!(1,3) - a1
        a2 = c!(2,1) - b1
        a3 = c!(3,1) - b1
    guard $  a2+b2 == (c!(2,2))
          && a2+b3 == (c!(2,3))
          && a3+b2 == (c!(3,2))
          && a3+b3 == (c!(3,3))
    return a1

main :: IO ()
main = do
    c <- listArray ((1,1),(3,3))
        . concatMap (map (fst . fromJust . BS.readInt) . BS.words)
        . BS.lines <$> BS.getContents :: IO (Array (Int,Int) Int)
    putStrLn $ if null (solve c) then "No" else "Yes"
