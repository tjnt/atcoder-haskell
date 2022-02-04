import Control.Monad
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

solve :: Int -> [Int] -> [Int]
solve n xs = map (\x -> g - ((b!x)-1)) xs
  where
    b = V.create $ do
        v <- VM.replicate (n+1) 0
        forM_ xs $ \x -> do
            VM.modify v succ x 
        return v
    g = V.foldl' (\a x -> a + ((x * (x-1)) `div` 2)) 0 b

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    mapM_ print $ solve n a
