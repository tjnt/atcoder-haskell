import Control.Monad
import Data.Array.IArray
import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as BS

type Matrix = Array (Int,Int) Int

main :: IO ()
main = do
    (n:b) <- map read . words <$> getLine :: IO [Int]
    let idx = ((1,1), (n,n))
    li <- replicateM n $ map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine :: IO [[Int]]
    ri <- replicateM n $ map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine :: IO [[Int]]
    let l = listArray idx $ concat li :: Matrix
        r = listArray idx $ concat ri :: Matrix
    let ans = solve b n l r
    forM_ (range idx) $ \(i,j) -> do
        putStr $ show (ans!(i,j))
        putChar $ if j == n then '\n' else ' '


solve :: [Int] -> Int -> Matrix -> Matrix -> Matrix
solve b n l r = solve'
  where
    b' = maximum b
    solve' = listArray ((1,1), (n,n)) $ f b' (range ((1,1),(n,n))) :: Matrix
    f _ [] = []
    f b (idx:xs) = let a = g b [(l!idx)..(r!idx)]
                  in a:f (a-b) xs
    g :: Int -> [Int] -> Int
    g b rng = minimumBy (\x y -> compare (abs (b-x)) (abs (b-y))) rng
