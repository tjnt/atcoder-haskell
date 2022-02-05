{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.IArray
import qualified Data.Sequence as Q
import Data.Sequence (ViewL (..), (<|), (|>))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Foldable

bfs :: Int -> Int -> Int -> Int -> V.Vector Int
bfs n x y s = runST $ do
    v <- VM.replicate (n+1) (-1)
    VM.write v s 0
    go v (s <| Q.empty)
    V.freeze v
  where
    go v (Q.viewl -> EmptyL) = return v
    go v (Q.viewl -> (p :< q)) = do
        c <- VM.read v p
        ns <- filterM (VM.read v >=> return . (== (-1)))
            . consIf (p==y) x
            . consIf (p==x) y
            . consIf (p > 1) (p-1)
            . consIf (p < n) (p+1)
            $ []
        mapM_ (\n -> VM.write v n (c+1)) ns
        go v (foldl' (|>) q ns)
    go _ _ = undefined

    consIf p x xs = if p then x:xs else xs

bucket :: Int -> V.Vector Int -> V.Vector Int
bucket n xs = V.create $ do
    v <- VM.replicate n 0
    V.mapM_ (VM.modify v succ) xs
    return v

solve :: Int -> Int -> Int -> V.Vector Int
solve n x y = V.map (`div`2) . V.tail . bucket n
            . V.concat . map (V.tail . bfs n x y) $ [1..n]

main :: IO ()
main = do
    [n,x,y] <- map read . words <$> getLine :: IO [Int]
    V.mapM_ print $ solve n x y
