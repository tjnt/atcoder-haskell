import qualified Data.ByteString.Char8 as BS
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

solve :: Array (Int,Int) Char -> (Int,Int) -> Bool
solve a s = runST $ do
    m <- thaw a :: ST st (STArray st (Int,Int) Char)
    dfs m s

dfs :: STArray st (Int,Int) Char -> (Int,Int) -> ST st Bool
dfs m (x,y) = do
    p <- readArray m (x,y)
    if p == 'g'
       then return True
       else do
           writeArray m (x,y) '#'
           vs <- filterM (chk m) ps
           case vs of
               [] -> return False
               _  -> or <$> mapM (dfs m) vs
  where
    ps = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]
    chk :: STArray st (Int,Int) Char -> (Int,Int) -> ST st Bool
    chk m (i,j) = do
        ((mnx,mny),(mxx,mxy)) <- getBounds m
        if mnx <= i && mny <= j && i <= mxx && j <= mxy
           then ('#'/=) <$> readArray m (i,j)
           else return False

main :: IO ()
main = do
    [h,w] <- map read . words <$> getLine :: IO [Int]
    c <- listArray ((0,0),(h-1,w-1))
         . concatMap BS.unpack . BS.lines
         <$> BS.getContents :: IO (Array (Int,Int) Char)
    let s = head [i | (i,e) <- assocs c, e == 's']
    putStrLn $ if solve c s then "Yes" else "No"
