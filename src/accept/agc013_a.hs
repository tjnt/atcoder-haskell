import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List

takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
takeWhile2 _ []  = []
takeWhile2 _ [x] = [x]
takeWhile2 p (i:j:xs) =
    if i `p` j then i : takeWhile2 p (j:xs) else [i]

solve :: [Int] -> Int
solve []  = 0
solve xs =
    let ml = maximum $ map length [x1,x2]
        r = drop ml xs
     in 1 + solve r
  where
    x1 = takeWhile2 (<=) xs
    x2 = takeWhile2 (>=) xs

main :: IO ()
main = do
    _ <- getLine
    a <- map (fst . fromJust . BS.readInt)
        . BS.words <$> BS.getLine :: IO [Int]
    print $ solve a
