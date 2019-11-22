import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

solve :: Integer -> [(Integer,Integer)] -> Integer
solve 0 _  = 0
solve _ [] = 0
solve m ((a,b):xs) = let c = min m b
                      in (c * a) + solve (m - c) xs

main :: IO ()
main = do
    [_,m] <- map read . words <$> getLine :: IO [Integer]
    ab <- sortOn fst . map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInteger) . BS.words) . BS.lines <$> BS.getContents
    print $ solve m ab
