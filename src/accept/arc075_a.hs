import Data.List
import Data.Maybe

main :: IO ()
main = do
    _ <- getLine
    s <- sort . map read . lines <$> getContents :: IO [Int]
    let s' = sum s
        m  = fromMaybe 0 $ find (\x -> rem10 x /= 0) s
    print $ if rem10 s' == 0
               then if rem10 (s'-m) == 0 then 0 else s'-m
               else s'
  where
    rem10 = (`rem` 10)
