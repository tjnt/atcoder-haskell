import qualified Data.Set as S

solve :: [String] -> Int
solve as@(w:ws) = f True S.empty ([head w]:as)
  where
    cmp w1 w2 = last w1 == head w2
    f :: Bool -> S.Set String -> [String] -> Int
    f b s [_] = 3
    f b s (w1:w2:ws)
      | cmp w1 w2 && S.notMember w2 s = f (not b) (S.insert w2 s) (w2:ws)
      | otherwise = if b then 2 else 1

main :: IO ()
main = do
    _ <- getLine
    w <- lines <$> getContents
    putStrLn $ case solve w of
                 1 -> "WIN"
                 2 -> "LOSE"
                 3 -> "DRAW"
