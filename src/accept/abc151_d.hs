{-# LANGUAGE ViewPatterns          #-}

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Array.IArray
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import           Data.Sequence         (ViewL (..), (<|), (|>))
import qualified Data.Sequence as Q
import Data.Foldable

type Maze = Array (Int,Int) Char

bfsMaze :: Maze -> (Int,Int) -> Array (Int,Int) Int
bfsMaze m s = runSTArray $ do
    a <- newListArray ((0,0),(h,w)) $ replicate ((h+1)*(w+1)) 0
    writeArray a s 1
    go a (s <| Q.empty)
    return a
  where
    ((_,_),(h,w)) = bounds m
    go :: STArray s (Int, Int) Int -> Q.Seq (Int, Int) -> ST s (STArray s (Int, Int) Int) 
    go a (Q.viewl -> EmptyL) = return a
    go a (Q.viewl -> (p :< q)) = do
        case next p of
            [] -> go a q
            ns -> do
                c <- readArray a p
                ns' <- filterM (readArray a >=> (\x -> return (x == 0))) ns
                mapM_ (\n -> writeArray a n (c+1)) ns'
                go a (foldl' (|>) q ns')
    go _ _ = undefined

    next (y,x) = [ (y',x') | (y',x') <- [(y+1,x), (y,x+1), (y-1,x), (y,x-1)]
                 , 0 <= y' && 0 <= x' && y' <= h && x' <= w && m!(y',x') /= '#'
                 ]

solve :: Int -> Int -> String -> Int
solve h w xs = maximum $ map (longest . bfsMaze maze) starts
  where
    maze = listArray ((0,0),(h-1,w-1)) xs :: Maze
    starts = [ (i,j) | i <- [0..h-1], j <- [0..w-1], maze!(i,j) /= '#']
    longest = maximum . elems

main :: IO ()
main = do
    [h,w] <- map read . words <$> getLine :: IO [Int]
    xs <- BS.unpack . BS.concat . BS.lines
      <$> BS.getContents :: IO String
    print . pred $ solve h w xs
