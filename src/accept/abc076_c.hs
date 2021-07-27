import Data.List
import Data.Bool

solve :: String -> String -> String
solve s t =
    case go rs of
        Just p ->
          let i = length s - length t  - p
              j = length s - p
              (s1,s2) = splitAt i s
              (_, s3) = splitAt j s
              s1' = replace '?' 'a' s1
              s3' = replace '?' 'a' s3
           in (s1' ++ t ++ s3')
        Nothing -> "UNRESTORABLE"
  where
    rs = reverse s
    rt = reverse t
    go as@(x:xs)
      | length t > length as = Nothing
      | match as = Just 0
      | otherwise = (+) <$> Just 1 <*> go xs
    match = all (uncurry f) . zip rt
    f a b
      | b == '?' = True
      | otherwise = a == b
    replace a b = map (\x -> bool x b (x==a))

main :: IO ()
main = do
    s <- getLine
    t <- getLine
    putStrLn $ solve s t
