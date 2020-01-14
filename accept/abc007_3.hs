{-# LANGUAGE ViewPatterns #-}

import           Data.Array.IArray
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import           Data.Sequence         (ViewL (..), (<|), (|>))
import qualified Data.Sequence         as Q

solve :: (Int,Int) -> (Int,Int) -> Array (Int,Int) Char -> Maybe Int
solve s@(sy,sx) g@(gy,gx) a = bfs (M.insert s 0 M.empty) (s <| Q.empty)
  where
    ((ly,lx),(hy,hx)) = bounds a
    bfs :: M.Map (Int,Int) Int -> Q.Seq (Int,Int) -> Maybe Int
    bfs _ (Q.viewl -> EmptyL) = Nothing
    bfs m (Q.viewl -> qq@(p :< q))
      | p == g    = Just (m M.! p)
      | otherwise =
          case next p of
              [] -> bfs m q
              ns -> let f a n = bimap (|> n) (M.insert n (succ (m M.! p))) a
                        (q',m') = foldl f (q,m) ns
                     in bfs m' q'
      where
        next (y,x) = [ (ny,nx) | (ny,nx) <- [(y+1,x), (y,x+1), (y-1,x), (y,x-1)]
                     ,  ly <= ny && lx <= nx && ny <= hy && nx <= hx
                     && a!(ny,nx) /= '#' && (ny,nx) `M.notMember` m ]

main :: IO ()
main = do
    [r,c] <- map read . words <$> getLine :: IO [Int]
    s <- toTuple . map read . words <$> getLine :: IO (Int,Int)
    g <- toTuple . map read . words <$> getLine :: IO (Int,Int)
    a <- listArray ((1,1),(r,c))
       . concatMap BS.unpack . BS.lines
     <$> BS.getContents :: IO (Array (Int,Int) Char)
    print $ fromJust $ solve s g a
  where
    toTuple [a,b] = (a,b)
