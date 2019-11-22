import qualified Data.ByteString.Char8 as BS
import Data.Array
import Data.List
import Control.Monad

calc :: Array Int (Double,Double) -> [Int] -> Double
calc a xs = let xs' = zip xs (tail xs)
             in foldl' f 0 xs'
  where
    f ac (i,j) = let (ix,iy) = a!i
                     (jx,jy) = a!j
                  in ac + (sqrt (((ix-jx)^2) + ((iy-jy)^2)))

main :: IO ()
main = do
    n <- readLn :: IO Int
    xy <- map ((\[a,b] -> (a,b)) . map ((read :: String -> Double) . BS.unpack) . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Double,Double)]

    let p = permutations [1..n]
        a = listArray (1,n) xy
    print $ (foldl' (\ac x -> ac + calc a x) 0 p) / (fromIntegral (length p) :: Double)
