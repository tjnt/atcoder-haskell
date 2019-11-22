import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

divableN :: Integral a => a -> a -> a
divableN n x
  | x `rem` n /= 0 = 0
  | otherwise      = 1 + divableN n (x `div` n)

solve :: [Int] -> Bool
solve a = let x = if c1 == 0 then 0 else 1
           in c0 + x <= c2 + 1
  where
    count p = length . filter p
    c0 = count (==0) a
    c1 = count (==1) a
    c2 = count (>=2) a

main :: IO ()
main = do
    _ <- getLine
    a <- map (divableN 2) . map (fst . fromJust . BS.readInt)
        . BS.words <$> BS.getLine :: IO [Int]
    putStrLn $ if solve a then "Yes" else "No"
