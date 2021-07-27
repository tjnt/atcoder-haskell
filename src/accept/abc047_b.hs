import           Control.Monad
main = do
    [w, h, n] <- map read . words <$> getLine
    v <- map (map read . words) <$> replicateM n getLine :: IO [[Int]]
    let m = [0, w, 0, h]
    print . g $ foldr f m v
  where
    f [x, y, a] [w1, w2, h1, h2] =
        case a of
            1 -> [max w1 x, w2, h1, h2]
            2 -> [w1, min w2 x, h1, h2]
            3 -> [w1, w2, max h1 y, h2]
            4 -> [w1, w2, h1, min h2 y]
    g [w1, w2, h1, h2] = max 0 (w2 - w1) * max 0 (h2 - h1)
