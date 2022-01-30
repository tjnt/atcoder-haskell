import Data.Sequence ((|>), (<|))
import qualified Data.Sequence as S
import Data.Foldable


solve :: [Int] -> [Int]
solve ss
  | even n = ss'
  | otherwise = reverse ss'
  where
    n = length ss
    f _ s [] = toList s
    f b s (a:as) =
        let s' | b         = s |> a
               | otherwise = a <| s
         in f (not b) s' as
    ss' = f True S.empty ss

main :: IO ()
main = do
    _ <- getLine
    as <- map read . words <$> getLine :: IO [Int]
    putStrLn . unwords . map show $ solve as
