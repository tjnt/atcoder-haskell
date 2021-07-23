import Data.Array
import Control.Monad

main :: IO ()
main = do
    c <- concat . concatMap words . lines <$> getContents :: IO String
    let a = listArray ((0,0),(3,3)) c
    let a' = array ((0,0),(3,3)) $
                [((3-i, 3-j), a!(i,j)) | i <- [0..3], j<- [0..3]]
    forM_ [0..3] $ \i -> do
        forM_ [0..3] $ \j -> do
            putChar $ a'!(i,j)
            when (j /= 3) $ putChar ' '
        putStrLn ""
