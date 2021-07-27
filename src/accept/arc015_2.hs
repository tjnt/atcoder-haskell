import qualified Data.ByteString.Char8 as BS
import Data.Bits
import Data.List
import Data.Array.IO
import Control.Monad

shift1 :: Int -> Int
shift1 = shift 1

bitOn :: Bool -> Int -> Int -> Int
bitOn c i x = if c then x .|. shift1 i else x

conditionsBit :: [Bool] -> Int
conditionsBit xs =
    foldl' (\a (i,c) -> bitOn c i a) 0 $ zip [0..] xs

solve :: (Double,Double) -> Int
solve (mx,mn) = conditionsBit cond
  where
    cond = [ 35 <= mx
           , 30 <= mx && mx < 35
           , 25 <= mx && mx < 30
           , 25 <= mn
           , mn < 0 && 0 <= mx
           , mx < 0
           ]

main :: IO ()
main = do
    _ <- getLine
    mt <- map ((\[a,b] -> (a,b)) . map ((read :: String -> Double) . BS.unpack) . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Double,Double)]
    a <- newListArray (0, 5) [0,0,0,0,0,0] :: IO (IOUArray Int Int)
    forM_ mt (\t -> do
        let b = solve t
        forM_ [0..5] (\i ->
            when (testBit b i) $ do
                x <- readArray a i
                writeArray a i (x+1))
            )
    (putStrLn . unwords . map show) =<< getElems a
