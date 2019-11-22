import           Control.Monad
import           Data.List
main = do
    x <- replicateM 5 readLn :: IO [Int]
    let a:aa = reverse $ sortOn (\i -> f i) x
    print $ sum (map (\i -> i + f i ) aa) + a
  where
    f n = if n `mod` 10 == 0 then 0 else 10 - n `mod` 10
