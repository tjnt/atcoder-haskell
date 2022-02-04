import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

solve :: Int -> Int -> [(Int,Int)] -> [Int]
solve n q xss = V.toList
              . V.map (\x -> if even x then 0 else 1)
              $ V.scanl1' (+) v
  where
    v :: V.Vector Int
    v = V.init . V.tail $ runST $ do
        v <- VM.replicate (n+2) 0
        forM_ xss $ \(l,r) -> do
            VM.modify v succ l
            VM.modify v pred (r+1)
        V.freeze v

main :: IO ()
main = do
    [n,q] <- map read . words <$> getLine :: IO [Int]
    xss <- map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInt) . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Int,Int)]
    putStrLn . concatMap show $ solve n q xss
