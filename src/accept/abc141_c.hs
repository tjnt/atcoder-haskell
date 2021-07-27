import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List
import qualified Data.IntMap as IM
import Control.Monad

main :: IO ()
main = do
    [n,k,q] <- map read . words <$> getLine :: IO [Int]
    a <- map (fst . fromJust . BS.readInt) . BS.lines <$> BS.getContents
    let a' = group $ sort a
        m  = IM.fromList $ map (\v -> (head v, length v)) a'
        ret = map (\i -> k - q + (maybe 0 id (IM.lookup i m))) [1..n]
    forM_ ret $ \n -> do
        putStrLn $ if n > 0 then "Yes" else "No" 
