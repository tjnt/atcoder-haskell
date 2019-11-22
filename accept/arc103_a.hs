import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List
import Data.Function

alternateSplit :: [a] -> ([a],[a])
alternateSplit = foldr (\x (l,r)-> (x:r,l)) ([],[])

solve :: [Int] -> Int
solve v
  | allSame v = length v `div` 2 
  | otherwise =
    let (l,r) = alternateSplit v
        ml1 = mode l
        ml2 = mode . filter (/=ml1) $ l
        mr1 = mode r
        mr2 = mode . filter (/=mr1) $ r
     in if ml1 /= mr1
           then cnt ml1 l + cnt mr1 r
           else min (cnt ml1 l + cnt mr2 r) (cnt ml2 l + cnt mr1 r)
  where
    allSame v = all (== head v) v
    mode = head . maximumBy (compare `on` length) . group . sort
    cnt c = length . filter (/=c)

main :: IO ()
main = do
    n <- readLn :: IO Int
    v <- map (fst . fromJust . BS.readInt)
        . BS.words <$> BS.getLine :: IO [Int]
    print $ solve v
