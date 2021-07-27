import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Array.IArray
import qualified Data.Set as Set

solve :: Array Int Int -> Int
solve a = f 1 Set.empty
  where
    f n s = case n of
                2 -> Set.size s
                _  | Set.member n s -> -1
                   | otherwise -> f (a!n) (Set.insert n s)

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map (fst . fromJust . BS.readInt) . BS.lines <$> BS.getContents
    print . solve $ listArray (1,n) a
