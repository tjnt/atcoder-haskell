import Control.Monad
import qualified Data.Map.Strict as M

countMap :: Ord a => [a] -> M.Map a Int
countMap xs = M.fromListWith (+) $ zip xs (repeat 1)

solve :: [String] -> [String] -> Int
solve s ts = maximum
           $ foldl f (countMap s) ts
  where
    f m t = M.update (Just . max 0 . pred) t m

main :: IO ()
main = do
    n <- readLn :: IO Int
    s <- replicateM n getLine :: IO [String]
    m <- readLn :: IO Int
    t <- replicateM m getLine :: IO [String]
    print $ solve s t
