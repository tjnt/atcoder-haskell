import Control.Monad
import Control.Monad.ST
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Data.Array.ST
import Data.Array.IArray

mapFromList :: Ord a => [(a,a)] -> M.Map a [a]
mapFromList []           = M.empty
mapFromList ((x1,x2):xs) = M.insertWith (\[a] b -> a:b) x1 [x2]
                         $ M.insertWith (\[a] b -> a:b) x2 [x1]
                         $ mapFromList xs

solve :: Int -> [(Int,Int)] -> [(Int,Int)] -> ST s (STUArray s Int Int)
solve n a p = do
    let m = mapFromList a
        ia = listArray (1,n) $ map (\i ->
            M.findWithDefault [] i m) [1..n] :: Array Int [Int]
    ma <- newArray (1,n) 0
    mapM_ (\(a,b) -> do
        v <- readArray ma a
        writeArray ma a (b + v)) p
    dfs ia ma 1 (-1)
    return ma
  where
    dfs :: Array Int [Int] ->  STUArray s Int Int -> Int -> Int -> ST s ()
    dfs ia ma i p = do
        pv <- readArray ma i
        forM_ (filter (/=p) (ia!i)) (\j -> do
            cv <- readArray ma j
            writeArray ma j (cv + pv)
            dfs ia ma j i)

main :: IO ()
main = do
    let tuple [a,b] = (a,b)
    [n,q] <- map read . words <$> getLine :: IO [Int]
    a <- replicateM (n-1) $ tuple
        . map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
    p <- replicateM q $ tuple
        . map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
    mapM_ print . elems $ runSTUArray $ solve n a p
