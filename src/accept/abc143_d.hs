import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List
import Data.Array.IArray

lowerBound:: (Integral i, Ix i, Ord e, IArray a e) => e -> a i e -> i
lowerBound x a =
    let (b,e) = bounds a in bsearch b (e + 1)
  where
    bsearch b e
      | e == b    = b
      | x <= a!p  = bsearch b p
      | otherwise = bsearch (p + 1) e
      where p = (b + e) `div` 2

main :: IO ()
main = do
    _ <- getLine
    l <- map (fst . fromJust . BS.readInt)
        . BS.words <$> BS.getLine :: IO [Int]
    let sl = listArray (0,length l - 1) $ sort l :: Array Int Int
    print $ sum
        [ let c = lowerBound ((sl!a)+(sl!b)) sl
           in c - (b+1)
          | a <- [0..length sl - 1], b <- [(a+1)..length sl - 1]
        ]
