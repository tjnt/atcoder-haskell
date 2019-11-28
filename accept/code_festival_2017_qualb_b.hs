import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List (sort, group)
import qualified Data.IntMap as M

solve :: [Int] -> [Int] -> Bool
solve ts ds =
    let f (t,l) = maybe False (l <=) (M.lookup t m)
     in all f tl
  where
    tpl g = (head g, length g)
    tl = map tpl . group . sort $ ts
    dl = map tpl . group . sort $ ds
    m  = M.fromList dl

main :: IO ()
main = do
    n <- readLn :: IO Int
    d <- map (fst . fromJust . BS.readInt)
        . BS.words <$> BS.getLine :: IO [Int]
    m <- readLn :: IO Int
    t <- map (fst . fromJust . BS.readInt)
        . BS.words <$> BS.getLine :: IO [Int]
    putStrLn $ if solve t d then "YES" else "NO"
