import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

solve :: [Int] -> (Int, Int)
solve xs = let (a,b) = partition (< 3200) xs
               a' = length . nub $ map (`div` 400) a
            in (max 1 a', a'+ length b)

main :: IO ()
main = do
    _ <- getLine
    a <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
    let (mn,mx) = solve a
    print mn
    print mx
