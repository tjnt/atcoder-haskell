import qualified Data.Map as M
import Data.List
import Data.Ord

countMap :: Ord a => [a] -> M.Map a Int
countMap xs = M.fromListWith (+) $ zip xs (repeat 1)

solve :: [String] -> [String]
solve xs = map fst $ filter ((==mx) . snd) xs'
  where
    xs' = M.toList $ countMap xs
    mx  = snd $ maximumBy (comparing snd) xs'

main :: IO ()
main = do
    _ <- getLine
    ss <- lines <$> getContents :: IO [String]
    mapM_ putStrLn $ solve ss
