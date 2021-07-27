-- 解答例を参考に解いた

import           Control.Monad

main = do
    n <- readLn :: IO Int
    a <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
    putStrLn $ if f ([0,0,0]:a) then "Yes" else "No"
  where
    f (_:[])  = True
    f (ai:aj:ax) =
        let [ti,xi,yi] = ai
            [tj,xj,yj] = aj
            dt = tj - ti
            dist = abs (xj - xi) + abs (yj - yi)
          in if dist > dt || odd dist /= odd dt
                then False else f (aj:ax)
