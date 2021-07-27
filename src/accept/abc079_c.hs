-- 解答例見た
import           Control.Monad
import           Data.Char
main = do
    s <- map digitToInt <$> getLine :: IO [Int]
    putStrLn . head $ f s
  where
    f [a,b,c,d] = do
        [(o1,s1), (o2,s2), (o3,s3)] <- replicateM 3 [((+), "+"),((-), "-")]
        guard (a `o1` b `o2` c `o3` d == 7)
        return $ show a ++ s1 ++ show b ++ s2 ++ show c ++ s3 ++ show d ++ "=7"
