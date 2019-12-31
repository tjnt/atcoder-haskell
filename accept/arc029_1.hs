import Data.List
import Data.Function

solve t = uncurry max . minimumBy (compare `on` (\(i,j) -> abs (i - j)))
        $ zip a b
  where
    a = map sum $ subsequences t
    b = map (sum t -) a

main :: IO ()
main = do
    _ <- getLine
    t <- map read . lines <$> getContents :: IO [Int]
    print $ solve t
