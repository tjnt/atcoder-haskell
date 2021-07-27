import qualified Data.Sequence as S

main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Int]
    let b = length a `rem` 2
    mapM_ print . fst $ foldl f (S.empty, b) a
  where
    f :: (S.Seq Int, Int) -> Int -> (S.Seq Int, Int)
    f (q,b) x
      | b == 0 = (q S.|> x, 1)
      | b == 1 = (x S.<| q, 0)
