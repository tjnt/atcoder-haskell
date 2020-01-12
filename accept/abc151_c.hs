import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Data.List

fromListWithConcat []         = M.empty
fromListWithConcat ((k,v):xs) = M.insertWith (\[a] b -> a:b) k [v] $ fromListWithConcat xs

solve :: [(Int,String)] -> (Int,Int)
solve ps = let (a,w) = go S.empty ps
            in (a,w - waOnly ps)
  where
    go :: S.IntSet -> [(Int,String)] -> (Int,Int)
    go acs [] = (0,0)
    go acs ((p,s):xs)
      | p `S.notMember` acs && s == "AC" =
          let (a,w) = go (S.insert p acs) xs in (a+1,w)
      | p `S.notMember` acs && s == "WA" =
          let (a,w) = go acs xs in (a,w+1)
      | otherwise   = go acs xs
    waOnly :: [(Int,String)] -> Int
    waOnly ps = let m = fromListWithConcat ps
                 in foldl f 0 m
    f a vs
      | all (=="WA") vs = a + length vs
      | otherwise = a

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    ps <- map ((\[a,b] -> (read a,b)) . map BS.unpack . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Int,String)]
    let (a,w) = solve ps
    putStr $ show a
    putStr " "
    print w
