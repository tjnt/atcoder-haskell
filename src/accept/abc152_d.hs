import Data.List
import Data.Maybe
import qualified Data.Map as M

digitNum :: Integral a => a -> a
digitNum = go 0
  where
    go i 0 = i
    go i x = go (i+1) (x `div` 10)

solve n = foldl f 0 $ map pair ns
  where
    ns = [1..n]
    m = M.fromListWith (+) $ map (\i -> (pair i,1)) ns
    pair i = (i `quot` 10^(digitNum i - 1), i `rem` 10)
    f a p = let p' = (snd p, fst p)
             in a + fromMaybe 0 (M.lookup p' m)

main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ solve n
