import           Control.Monad
import           Data.List
main = do
    n <- readLn :: IO Int
    l <- map words <$> replicateM n getLine :: IO [[String]]
    let xs = map (\(t1, t2) -> (t1, t2!!0, (read (t2!!1) :: Int))) (zip [1..] l)
    mapM_ print .  map sel1 . join . map (sortBy (flip cmp3)) . groupBy eq2 . sortBy cmp2 $ xs
  where
    sel1 (x,_,_) = x
    sel2 (_,x,_) = x
    sel3 (_,_,x) = x
    eq2 a b = sel2 a == sel2 b
    cmp2 a b = compare (sel2 a) (sel2 b)
    cmp3 a b = compare (sel3 a) (sel3 b)
