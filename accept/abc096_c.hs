import qualified Data.ByteString.Char8 as BS
import Data.Array.IArray
import Control.Monad

main :: IO ()
main = do
    [h,w] <- map read . words <$> getLine :: IO [Int]
    s <- BS.unpack . BS.concat . BS.lines <$> BS.getContents
    let a = listArray ((0,0),(w-1,h-1)) s :: Array (Int,Int) Char
    let ans = do
        i <- [0..w-1]
        j <- [0..h-1]
        guard $ a!(i,j) == '#'
        return $ i > 0 && a!(i-1,j) == '#'
              || i < w-1 && a!(i+1,j) == '#'
              || j > 0 && a!(i,j-1) == '#'
              || j < h-1 && a!(i,j+1) == '#'
    putStrLn $
        if all (==True) ans then "Yes" else "No"
