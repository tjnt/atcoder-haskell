import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List (length, sort, group)
import Data.Array.Unboxed

solve :: [Int] -> Int
solve xs = foldr f 0 xs
  where
    (b, e) = (head xs, last xs)
    gs = group xs
    zs = zip (map head gs) (map length gs) ++ [(e+1,0), (e+2,0)]
    aa = accumArray (+) 0 (b, e+2) zs :: UArray Int Int
    f x a = let a' = aa!x + aa!(x+1) + aa!(x+2) in max a a'

main :: IO ()
main = do
    _ <- getLine
    a <- sort . map (fst . fromJust . BS.readInt)
        . BS.words <$> BS.getLine :: IO [Int]
    print $ solve a
