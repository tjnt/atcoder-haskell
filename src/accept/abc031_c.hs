import qualified Data.Vector as V
import Data.List

solve :: V.Vector Int -> Int
solve aa = maximum
         $ [ countR aa i j even
           | (i,j,_) <- select ]
  where
    n = V.length aa
    select = do
        i <- [0..n-1]
        let v = maximumBy comp $ do
            j <- [ j | j <- [0..n-1], i /= j]
            return (i, j, countR aa i j odd)
        return v
    comp (_,j1,c1) (_,j2,c2)
      | c1 /= c2 = c1 `compare` c2
      | otherwise = j2 `compare` j1

countR v i j = count v'
  where v' | i < j      = V.slice i (j-i+1) v
           | otherwise  = V.slice j (i-j+1) v
count v p = V.sum $ V.ifilter (\i x -> p i) v

main :: IO ()
main = do
    _ <- getLine
    aa <- V.fromList . map read . words <$> getLine :: IO (V.Vector Int)
    print $ solve aa
