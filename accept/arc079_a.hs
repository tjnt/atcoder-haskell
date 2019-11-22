import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Array.IArray

adjacencyList :: (Ix a) => a -> a -> [(a,a)] -> Array a [a]
adjacencyList bgn end = accumArray (flip (:)) [] (bgn,end)

solve :: Array Int [Int] -> Int -> Bool
solve g n = elem n p2
  where
    p1 = g!1
    p2 = concat [ g!p | p <- p1 ]

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    ab <- map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInt) . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Int,Int)]
    let g = adjacencyList 1 (max n m) ab
    putStrLn $ if solve g n then "POSSIBLE" else "IMPOSSIBLE"
